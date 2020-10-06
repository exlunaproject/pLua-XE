unit LuaWrapper;

{
  Copyright (c) 2007 Jeremy Darling
  Modifications copyright (c) 2010-2014 Felipe Daragon
  
  License: MIT (http://opensource.org/licenses/mit-license.php)
  
  Changes:
  * 19.06.2014, FD - Changed to work with string instead of ansistring.
  * 16.05.2010, FD - Added LocateCall & Push.
  * 06.05.2013, FD - Added support for Delphi XE2 or higher.
  * 19.05.2014, FD - Added backwards compatibility with non-unicode Delphi.
}

interface

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$I Lua.inc}

{$DEFINE TLuaAsComponent}
{$DEFINE TLuaHandlersAsIsObjectType}

uses
  Classes, SysUtils, Variants, Types, Lua, pLua;

type
  TLua = class;

  TLuaOnException = procedure( Title: string; Line: Integer; Msg: string;
                               var handled : Boolean) {$IFDEF TLuaHandlersAsIsObjectType}of object{$ENDIF};
  TLuaOnLoadLibs  = procedure( LuaWrapper : TLua ) {$IFDEF TLuaHandlersAsIsObjectType}of object{$ENDIF};
  
  { TLUA }
  TLUA=class{$IFDEF TLuaAsComponent}(TComponent){$ENDIF}
  private
    FOnException: TLuaOnException;
    FOnLoadLibs: TLuaOnLoadLibs;
    FUseDebug: Boolean;
    FScript: String;
    FLibFile: String;
    FLibName: String;
    FMethods : TStringList;
    L : Plua_State;
    function  GetLuaCPath: String;
    function  GetLuaPath: String;
    function  GetValue(valName : String): Variant;
    procedure SetLibName(const Value: String);
    procedure SetLuaCPath(const AValue: String);
    procedure SetLuaPath(const AValue: String);
    procedure OpenLibs;
    procedure SetOnException(const AValue: TLuaOnException);
    procedure SetOnLoadLibs(const AValue: TLuaOnLoadLibs);
    procedure SetUseDebug(const AValue: Boolean);
    procedure ErrorTest(errCode : Integer);
    procedure HandleException(E : LuaException);
    procedure SetValue(valName : String; const AValue: Variant);
  public
    constructor Create{$IFDEF TLuaAsComponent}(anOwner : TComponent); override {$ENDIF};
    //{$IFDEF TLuaAsComponent}constructor Create; {$ENDIF}
    destructor Destroy; override;

    procedure Close;
    procedure Open;

    procedure LoadScript(Script : String);
    procedure LoadFile(FileName: String);
    procedure Execute;
    procedure ExecuteCmd(Script:String);
    procedure ExecuteFile(FileName : String);
    procedure RegisterLuaMethod(aMethodName: String; Func: lua_CFunction);
    procedure RegisterLuaTable(PropName: String; reader: lua_CFunction; writer : lua_CFunction = nil);
    function  FunctionExists(aMethodName: String) : Boolean;
    function  CallFunction( FunctionName : String; const Args: array of Variant;
                            Results : PVariantArray = nil):Integer;
    function  TableFunctionExists(TableName, FunctionName : String; out tblidx : Integer) : Boolean; overload;
    function  TableFunctionExists(TableName, FunctionName : String) : Boolean; overload;
    function  CallTableFunction( TableName, FunctionName : String;
                               const Args: array of Variant;
                               Results : PVariantArray = nil):Integer;
                               
    procedure LocateCall(const AMethod:String);
    procedure Push(AParam:Variant);

    property LibName  : String read FLibName write SetLibName;
    property LuaState : Plua_State read L write L; // FD: 16/05/2010, added write
    property LuaPath  : String read GetLuaPath write SetLuaPath;
    property LuaCPath : String read GetLuaCPath write SetLuaCPath;
    property UseDebug : Boolean read FUseDebug write SetUseDebug;
    property Value[valName : String] : Variant read GetValue write SetValue; default;
    property OnException : TLuaOnException read FOnException write SetOnException;
    property OnLoadLibs : TLuaOnLoadLibs read FOnLoadLibs write SetOnLoadLibs;
  end;

  { TLUAThread }
  TLUAThread=class
  private
    FMaster : TLUA;
    FMethodName: String;
    FTableName: String;
    L : PLua_State;
    FThreadName : String;
    function GetIsValid: Boolean;
  public
    constructor Create(LUAInstance: TLUA; ThreadName : String);
    destructor Destroy; override;

    function Start(TableName : String; AMethodName : String; const ArgNames: array of String; var ErrorString : String) : Boolean;
    function Resume(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : String) : Boolean;

    property LuaState : Plua_State read L;
    property IsValid : Boolean read GetIsValid;
    property ThreadName : String read FThreadName;
    //property MethodName : String read FMethodName;
    property TableName  : String read FTableName;
  end;

  { TLUAThreadList }
  TLUAThreadList=class
  private
    FThreads : TList;
    FLUAInstance : TLUA;
    function GetCount: Integer;
    function GetThread(index: integer): TLUAThread;
  public
    constructor Create(LUAInstance: TLUA);
    destructor Destroy; override;

    procedure Process(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : String);

    function SpinUp(TableName, AMethodName, ThreadName : String; var ErrorString : String) : Boolean;
    function IndexOf(ThreadName : String): Integer;
    procedure Release(ThreadIndex : Integer);

    property Thread[index:integer]: TLUAThread read GetThread;
    property Count : Integer read GetCount;
  end;

implementation

uses
  pLuaObject, pLuaRecord;
 
constructor TLUA.Create{$IFDEF TLuaAsComponent}(anOwner: TComponent){$ENDIF};
begin
  {$IFDEF TLuaAsComponent}inherited;{$ENDIF}
  FUseDebug := false;
  FMethods := TStringList.Create;
  Open;
end;

destructor TLUA.Destroy;
begin
  Close;
  FMethods.Free;
  inherited;
end;

procedure TLUA.LocateCall(const AMethod:String);
begin
  lua_getglobal(L,PAnsiChar(AnsiString(AMethod)));
end;

procedure TLUA.Push(AParam:Variant);
var AText:String;
begin
  case VarType(AParam) of
    varByte,varWord,
    varShortInt,varSmallInt,
    varInteger:lua_pushnumber(L,Integer(AParam));
    varDouble:lua_pushnumber(L,Double(AParam));
    varString:begin
               AText:=VarToStr(AParam);
               lua_pushstring(L,AText);
             end;
    varOLEStr:begin
                AText:=string(UTF8EnCode(VarToWideStr(AParam)));
                lua_pushstring(L,AText);
              end;
    varBoolean:lua_pushboolean(L,(AParam = True));
    //else LastError:=letPush; // ToDo, restore later
  end;
end;

procedure TLUA.Execute;
begin
  if L = nil then
    Open;
  if FScript <> EmptyStr then
    ErrorTest(luaL_loadbuffer(L, PAnsiChar(ansistring(FScript)), length(ansistring(FScript)), PAnsiChar(AnsiString(LibName))))
  else
    if FLibFile <> EmptyStr then
      ErrorTest(luaL_loadfile(L, PAnsiChar(ansistring(FLibFile))))
    else
      exit;
  ErrorTest(lua_pcall(L, 0, 0, 0));
end;

procedure TLUA.ExecuteCmd(Script: String);
begin
  if L= nil then
    Open;
  ErrorTest(luaL_loadbuffer(L, PAnsiChar(AnsiString(Script)), Length(AnsiString(Script)), PAnsiChar(AnsiString(LibName))));
  ErrorTest(lua_pcall(L, 0, 0, 0));
end;

procedure TLUA.ExecuteFile(FileName: String);
//var
//  Script : String;
//  sl     : TStringList;
begin
  if L = nil then
    Open;

  ErrorTest(luaL_loadfile(L, PAnsiChar(AnsiString(FileName))));
{
  if L = nil then
    Open;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    Script := sl.Text;
  finally
    sl.Free;
  end;
  ErrorTest(luaL_loadbuffer(L, PAnsiChar(AnsiString(Script)), Length(AnsiString(Script)), PAnsiChar(AnsiString(LibName))));   }
  ErrorTest(lua_pcall(L, 0, 0, 0));
end;

procedure TLUA.SetLuaPath(const AValue: String);
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'path');
  lua_pushstring(L, AValue);
  lua_settable(L, -3);
end;

procedure TLUA.LoadFile(FileName: String);
begin
  if L = nil then
    Open;
  FLibFile := FileName;
  FScript := EmptyStr;
  luaL_loadfile(L, PAnsiChar(AnsiString(FileName)));
end;

procedure TLUA.LoadScript(Script: String);
begin
  if FScript <> Script then
    Close;
  if L = nil then
    Open;
  FScript := Trim(Script);
  FLibFile := EmptyStr;
  if FScript <> EmptyStr then
    luaL_loadbuffer(L, PAnsiChar(AnsiString(Script)), length(AnsiString(Script)), PAnsiChar(AnsiString(LibName)));
end;

function TLUA.FunctionExists(aMethodName: String): Boolean;
begin
  lua_pushstring(L, aMethodName);
  lua_rawget(L, LUA_GLOBALSINDEX);
  result := (not lua_isnil(L, -1)) and lua_isfunction(L, -1);
  lua_pop(L, 1);
end;

procedure TLUA.RegisterLUAMethod(aMethodName: String; Func: lua_CFunction);
begin
  if L = nil then
    Open;
  lua_register(L, aMethodName, Func);
  if FMethods.IndexOf(aMethodName) = -1 then
    FMethods.AddObject(aMethodName, TObject(@Func))
  else
    FMethods.Objects[FMethods.IndexOf(aMethodName)] := TObject(@Func);
end;

procedure TLUA.RegisterLuaTable(PropName: String; reader: lua_CFunction;
  writer: lua_CFunction);
begin
  plua_RegisterLuaTable(l, PropName, reader, writer);
end;

procedure TLUA.SetLibName(const Value: String);
begin
  FLibName := Value;
end;

procedure TLUA.SetLuaCPath(const AValue: String);
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'cpath');
  lua_pushstring(L, AValue);
  lua_settable(L, -3);
end;

function TLUA.GetLuaPath: String;
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'path');
  lua_rawget(L, -2);
  result := lua_tostring(L, -1);
end;

function TLUA.GetValue(valName : String): Variant;
begin
  result := NULL;
  lua_pushstring(l, valName);
  lua_rawget(l, LUA_GLOBALSINDEX);
  try
    result := plua_tovariant(l, -1);
  finally
    lua_pop(l, 1);
  end;
end;

function TLUA.GetLuaCPath: String;
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'cpath');
  lua_rawget(L, -2);
  result := lua_tostring(L, -1);
end;

function TLUA.CallFunction(FunctionName: String;
  const Args: array of Variant; Results: PVariantArray = nil): Integer;
begin
  result := -1;
  try
    if FunctionExists(FunctionName) then
      result := plua_callfunction(L, FunctionName, Args, Results)
    else
      result := -1;
  except
    on E:LuaException do
      HandleException(E);
  end;
end;

procedure TLUA.Close;
begin
  if L <> nil then
    begin
      plua_ClearObjects(L);
      plua_ClearRecords(L);
      lua_close(L);
    end;
  L := nil;
end;

procedure TLUA.Open;
begin
  if L <> nil then
    Close;
  L := lua_open;
  OpenLibs;
end;

procedure TLUA.OpenLibs;
var
  I : Integer;
begin
  luaL_openlibs(L);
  if UseDebug then
    luaopen_debug(L);
  lua_settop(L, 0);

  for I := 0 to FMethods.Count -1 do
    RegisterLUAMethod(FMethods[I], lua_CFunction(Pointer(FMethods.Objects[I])));

  RecordTypesList.RegisterTo(L);
  ClassTypesList.RegisterTo(L);

  if assigned(FOnLoadLibs) then
    FOnLoadLibs(self);
end;

procedure TLUA.SetOnException(const AValue: TLuaOnException);
begin
  if @FOnException=@AValue then exit; // FD: 16/05/2010, changed to @AValue
  FOnException:=AValue;
end;

procedure TLUA.SetOnLoadLibs(const AValue: TLuaOnLoadLibs);
begin
  if @FOnLoadLibs=@AValue then exit; // FD: 16/05/2010, changed to @AValue
  FOnLoadLibs:=AValue;
  //if (L <> nil) and (FOnLoadLibs <> nil) then
  if (L <> nil) and (assigned(FOnLoadLibs)) then
    FOnLoadLibs(self);
end;

procedure TLUA.SetUseDebug(const AValue: Boolean);
begin
  if FUseDebug=AValue then exit;
  FUseDebug:=AValue;
end;

procedure TLUA.ErrorTest(errCode: Integer);
var
  msg : String;
begin
  if errCode <> 0 then
    begin
      msg := lua_tostring(l, -1);
      lua_pop(l, 1);
      HandleException(LuaException.Create(msg));
    end;
end;

procedure TLUA.HandleException(E: LuaException);
var
  title, msg : String;
  line       : Integer;
  handled    : Boolean;
begin
  handled := false;
  if assigned(FOnException) then
    begin
      plua_spliterrormessage(e.Message, title, line, msg);
      FOnException(title, line, msg, handled);
    end;
  if not handled then
    raise E;
end;

procedure TLUA.SetValue(valName : String; const AValue: Variant);
begin
  if VarIsType(AValue, varString) then
    begin
      lua_pushliteral(l, valName);
      lua_pushstring(l, String(AValue));
      lua_settable(L, LUA_GLOBALSINDEX);
    end
  else
    begin
      lua_pushliteral(l, valName);
      plua_pushvariant(l, AValue);
      lua_settable(L, LUA_GLOBALSINDEX);
    end;
end;

function TLUA.CallTableFunction(TableName, FunctionName: String;
  const Args: array of Variant; Results: PVariantArray): Integer;
var
  tblidx : integer;
begin
  result := -1;
  try
    if TableFunctionExists(TableName, FunctionName, tblidx) then
      begin
        lua_pushvalue(l, tblidx);
        tblidx := lua_gettop(l);
        result := plua_callfunction(l, FunctionName, args, results, tblidx)
      end
    else
      result := -1;
  except
    on E: LuaException do
      HandleException(E);
  end;
end;

function TLUA.TableFunctionExists(TableName,
  FunctionName: String; out tblidx : Integer): Boolean;
begin
  lua_pushstring(L, TableName);
  lua_rawget(L, LUA_GLOBALSINDEX);
  result := lua_istable(L, -1);
  if result then
    begin
      tblidx := lua_gettop(L);
      lua_pushstring(L, FunctionName);
      lua_rawget(L, -2);
      result := lua_isfunction(L, -1);
      lua_pop(L, 1);
    end
  else
    begin
      tblidx := -1;
      lua_pop(L, 1);
    end;
end;

function TLUA.TableFunctionExists(TableName, FunctionName: String
  ): Boolean;
var
  tblidx : Integer;
begin
  result := TableFunctionExists(TableName, FunctionName, tblidx);
  if result then
    lua_pop(L, 1);
end;

{ TLUAThread }

function TLUAThread.GetIsValid: Boolean;
begin
  lua_getglobal(L, PAnsiChar(AnsiString(FThreadName)));
  result := not lua_isnil(L, 1);
  lua_pop(L, 1);
end;

constructor TLUAThread.Create(LUAInstance: TLUA; ThreadName: String);
begin
  L := lua_newthread(LUAInstance.LuaState);
  FThreadName := ThreadName;
  lua_setglobal(LUAInstance.LuaState, PAnsiChar(AnsiString(ThreadName)));
  FMaster := LUAInstance;
end;

destructor TLUAThread.Destroy;
begin
  lua_pushnil(FMaster.LuaState);
  lua_setglobal(FMaster.LuaState, PAnsiChar(AnsiString(FThreadName)));
  inherited;
end;

function luaResume(L : PLua_State; NArgs:Integer; out Res : Integer) : Boolean;
begin
  Res := lua_resume(L, NArgs);
  result := Res <> 0;
end;

function TLUAThread.Start(TableName : String; AMethodName : String; const ArgNames: array of String; var ErrorString : String) : Boolean;
var
  i,
  rres : Integer;
begin
  FTableName := TableName;
  FMethodName := AMethodName;
  if TableName <> EmptyStr then
    begin
      lua_pushstring(L, TableName);
      lua_gettable(L, LUA_GLOBALSINDEX);
      lua_pushstring(L, AMethodName);
      lua_rawget(L, -2);
    end
  else
    lua_getglobal(L, PAnsiChar(AnsiString(AMethodName)));

  for i := 0 to Length(ArgNames)-1 do
    lua_getglobal(L, PAnsiChar(AnsiString(ArgNames[i])));

  if luaResume(L, Length(ArgNames), rres) then
    begin
      ErrorString := lua_tostring(L, -1);
      result := false;
      exit;
    end
  else
    result := true;
end;

function TLUAThread.Resume(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : String) : Boolean;
var
  rres,
  i : Integer;
  msg : String;
begin
  lua_pushnumber(L, EllapsedTime);
  for i := 0 to Length(Args)-1 do
    plua_pushvariant(L, Args[i]);
  if luaResume(L, Length(Args)+1, rres) then
    begin
      ErrorString := lua_tostring(L, -1);
      msg := 'Error ('+IntToStr(rres)+'): '+ErrorString;
      result := false;
      if result=false then begin // hide weird H2077 compiler warning
      end;
      raise exception.Create(msg);
    end
  else
    result := true;
end;

{ TLUAThreadList }

function TLUAThreadList.GetCount: Integer;
begin
  result := FThreads.Count;
end;

function TLUAThreadList.GetThread(index: integer): TLUAThread;
begin
  result := TLUAThread(FThreads[index]);
end;

constructor TLUAThreadList.Create(LUAInstance: TLUA);
begin
  FLUAInstance := LUAInstance;
  FThreads := TList.Create;
end;

destructor TLUAThreadList.Destroy;
var
  T : TLUAThread;
begin
  while FThreads.Count > 0 do
    begin
      T := TLUAThread(FThreads[FThreads.Count-1]);
      FThreads.Remove(T);
      T.Free;
    end;
  FThreads.Free;
  inherited;
end;

procedure TLUAThreadList.Process(EllapsedTime: lua_Number; Args : array of Variant;
  var ErrorString: String);
var
  i : Integer;
begin
  i := 0;
  while i < Count do
    begin
      if not TLUAThread(FThreads[I]).Resume(EllapsedTime, Args, ErrorString) then
        Release(i)
      else
        inc(i);
    end;
end;

function TLUAThreadList.SpinUp(TableName, AMethodName, ThreadName: String; var ErrorString : String) : Boolean;
var
  T : TLUAThread;
begin
  T := TLUAThread.Create(FLUAInstance, ThreadName);
  FThreads.Add(T);
  result := T.Start(TableName, AMethodName, [], ErrorString);
end;

function TLUAThreadList.IndexOf(ThreadName: String): Integer;
var
  i : Integer;
begin
  result := -1;
  i := 0;
  while (result = -1) and (i<FThreads.Count) do
    begin
      if CompareText(ThreadName, TLUAThread(FThreads[i]).ThreadName) = 0 then
        result := i;
      inc(i);
    end;
end;

procedure TLUAThreadList.Release(ThreadIndex: Integer);
var
  T : TLUAThread;
begin
  if (ThreadIndex < Count) and (ThreadIndex > -1) then
    begin
      T := TLUAThread(FThreads[ThreadIndex]);
      FThreads.Delete(ThreadIndex);
      T.Free;
    end;
end;

initialization

finalization

end.
