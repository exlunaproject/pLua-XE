unit LuaWrapper;

(*
 * FD 2010 - Added LocateCall & Push
 * FD 2013 - Updated for XE3
 *)

interface

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$DEFINE TLuaAsComponent}
{$DEFINE TLuaHandlersAsIsObjectType}

uses
  Classes,
  lua,
  pLua;

type
  TLua = class;

  TLuaOnException = procedure( Title: ansistring; Line: Integer; Msg: ansistring;
                               var handled : Boolean) {$IFDEF TLuaHandlersAsIsObjectType}of object{$ENDIF};
  TLuaOnLoadLibs  = procedure( LuaWrapper : TLua ) {$IFDEF TLuaHandlersAsIsObjectType}of object{$ENDIF};
  
  { TLUA }
  TLUA=class{$IFDEF TLuaAsComponent}(TComponent){$ENDIF}
  private
    FOnException: TLuaOnException;
    FOnLoadLibs: TLuaOnLoadLibs;
    FUseDebug: Boolean;
    L : Plua_State;
    FScript,
    FLibFile,
    FLibName: AnsiString;
    FMethods : TStringList;
    function  GetLuaCPath: AnsiString;
    function  GetLuaPath: AnsiString;
    function  GetValue(valName : AnsiString): Variant;
    procedure SetLibName(const Value: AnsiString);
    procedure SetLuaCPath(const AValue: AnsiString);
    procedure SetLuaPath(const AValue: AnsiString);
    procedure OpenLibs;
    procedure SetOnException(const AValue: TLuaOnException);
    procedure SetOnLoadLibs(const AValue: TLuaOnLoadLibs);
    procedure SetUseDebug(const AValue: Boolean);
    procedure ErrorTest(errCode : Integer);
    procedure HandleException(E : LuaException);
    procedure SetValue(valName : AnsiString; const AValue: Variant);
  public
    constructor Create{$IFDEF TLuaAsComponent}(anOwner : TComponent); override {$ENDIF};
    //{$IFDEF TLuaAsComponent}constructor Create; {$ENDIF}
    destructor Destroy; override;

    procedure Close;
    procedure Open;

    procedure LoadScript(Script : AnsiString);
    procedure LoadFile(FileName:AnsiString);
    procedure Execute;
    procedure ExecuteCmd(Script:AnsiString);
    procedure ExecuteFile(FileName : AnsiString);
    procedure RegisterLuaMethod(aMethodName: AnsiString; Func: lua_CFunction);
    procedure RegisterLuaTable(PropName: AnsiString; reader: lua_CFunction; writer : lua_CFunction = nil);
    function  FunctionExists(aMethodName:AnsiString) : Boolean;
    function  CallFunction( FunctionName :AnsiString; const Args: array of Variant;
                            Results : PVariantArray = nil):Integer;
    function  TableFunctionExists(TableName, FunctionName : AnsiString; out tblidx : Integer) : Boolean; overload;
    function  TableFunctionExists(TableName, FunctionName : AnsiString) : Boolean; overload;
    function  CallTableFunction( TableName, FunctionName :AnsiString;
                               const Args: array of Variant;
                               Results : PVariantArray = nil):Integer;
                               
    procedure LocateCall(const AMethod:String); // FD: 16/05/2010, added
    procedure Push(AParam:Variant); // FD: 16/05/2010, added

    property LibName  : AnsiString read FLibName write SetLibName;
    property LuaState : Plua_State read L write L; // FD: 16/05/2010, added write
    property LuaPath  : AnsiString read GetLuaPath write SetLuaPath;
    property LuaCPath : AnsiString read GetLuaCPath write SetLuaCPath;
    property UseDebug : Boolean read FUseDebug write SetUseDebug;
    property Value[valName : AnsiString] : Variant read GetValue write SetValue; default;
    property OnException : TLuaOnException read FOnException write SetOnException;
    property OnLoadLibs : TLuaOnLoadLibs read FOnLoadLibs write SetOnLoadLibs;
  end;

  { TLUAThread }
  TLUAThread=class
  private
    FMaster : TLUA;
    FMethodName: AnsiString;
    FTableName: AnsiString;
    L : PLua_State;
    FThreadName : AnsiString;
    function GetIsValid: Boolean;
  public
    constructor Create(LUAInstance: TLUA; ThreadName : AnsiString);
    destructor Destroy; override;

    function Start(TableName : AnsiString; AMethodName : AnsiString; const ArgNames: array of AnsiString; var ErrorString : AnsiString) : Boolean;
    function Resume(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : AnsiString) : Boolean;

    property LuaState : Plua_State read L;
  published
    property IsValid : Boolean read GetIsValid;
    property ThreadName : AnsiString read FThreadName;
    property MethodName : AnsiString read FMethodName;
    property TableName  : AnsiString read FTableName;
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

    procedure Process(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : AnsiString);

    function SpinUp(TableName, AMethodName, ThreadName : AnsiString; var ErrorString : AnsiString) : Boolean;
    function IndexOf(ThreadName : AnsiString): Integer;
    procedure Release(ThreadIndex : Integer);

    property Thread[index:integer]: TLUAThread read GetThread;
  published
    property Count : Integer read GetCount;
  end;

implementation

uses
  Variants,
  SysUtils,
  pLuaObject,
  pLuaRecord;
 
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

procedure TLUA.LocateCall(const AMethod:String); // FD: 16/05/2010, added
begin
  lua_getglobal(L,PAnsiChar(AMethod));
end;

procedure TLUA.Push(AParam:Variant); // FD: 16/05/2010, added
var AText:String;
begin
  case VarType(AParam) of
    varByte,varWord,
    varShortInt,varSmallInt,
    varInteger:lua_pushnumber(L,Integer(AParam));
    varDouble:lua_pushnumber(L,Double(AParam));
    varString:begin
               AText:=VarToStr(AParam);
               lua_pushstring(L,PAnsiChar(AText));
             end;
    varOLEStr:begin
                AText:=UTF8EnCode(VarToWideStr(AParam));
                lua_pushstring(L,PAnsiChar(AText));
              end;
    varBoolean:lua_pushboolean(L,(AParam = True));
    //else LastError:=letPush; // FD: 16/05/2010, ToDo, restore later
  end;
end;

procedure TLUA.Execute;
begin
  if L = nil then
    Open;
  if FScript <> '' then
    ErrorTest(luaL_loadbuffer(L, PAnsiChar(FScript), length(FScript), PAnsiChar(LibName)))
  else
    if FLibFile <> '' then
      ErrorTest(luaL_loadfile(L, PAnsiChar(FLibFile)))
    else
      exit;
  ErrorTest(lua_pcall(L, 0, 0, 0));
end;

procedure TLUA.ExecuteCmd(Script: AnsiString);
begin
  if L= nil then
    Open;
  ErrorTest(luaL_loadbuffer(L, PAnsiChar(Script), Length(Script), PAnsiChar(LibName)));
  ErrorTest(lua_pcall(L, 0, 0, 0));
end;

procedure TLUA.ExecuteFile(FileName: AnsiString);
var
  Script : AnsiString;
  sl     : TStringList;
begin
  if L = nil then
    Open;

  ErrorTest(luaL_loadfile(L, PAnsiChar(FileName)));
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
  ErrorTest(luaL_loadbuffer(L, PAnsiChar(Script), Length(Script), PAnsiChar(LibName)));   }
  ErrorTest(lua_pcall(L, 0, 0, 0));
end;

procedure TLUA.SetLuaPath(const AValue: AnsiString);
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'path');
  lua_pushstring(L, PAnsiChar(AValue));
  lua_settable(L, -3);
end;

procedure TLUA.LoadFile(FileName: AnsiString);
begin
  if L = nil then
    Open;
  FLibFile := FileName;
  FScript := '';
  luaL_loadfile(L, PAnsiChar(FileName));
end;

procedure TLUA.LoadScript(Script: AnsiString);
begin
  if FScript <> Script then
    Close;
  if L = nil then
    Open;
  FScript := Trim(Script);
  FLibFile := '';
  if FScript <> '' then
    luaL_loadbuffer(L, PAnsiChar(Script), length(Script), PAnsiChar(LibName));
end;

function TLUA.FunctionExists(aMethodName: AnsiString): Boolean;
begin
  lua_pushstring(L, PAnsiChar(aMethodName));
  lua_rawget(L, LUA_GLOBALSINDEX);
  result := (not lua_isnil(L, -1)) and lua_isfunction(L, -1);
  lua_pop(L, 1);
end;

procedure TLUA.RegisterLUAMethod(aMethodName: AnsiString; Func: lua_CFunction);
begin
  if L = nil then
    Open;
  lua_register(L, PAnsiChar(aMethodName), Func);
  if FMethods.IndexOf(aMethodName) = -1 then
    FMethods.AddObject(aMethodName, TObject(@Func))
  else
    FMethods.Objects[FMethods.IndexOf(aMethodName)] := TObject(@Func);
end;

procedure TLUA.RegisterLuaTable(PropName: AnsiString; reader: lua_CFunction;
  writer: lua_CFunction);
begin
  plua_RegisterLuaTable(l, PropName, reader, writer);
end;

procedure TLUA.SetLibName(const Value: AnsiString);
begin
  FLibName := Value;
end;

procedure TLUA.SetLuaCPath(const AValue: AnsiString);
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'cpath');
  lua_pushstring(L, PAnsiChar(AValue));
  lua_settable(L, -3);
end;

function TLUA.GetLuaPath: AnsiString;
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'path');
  lua_rawget(L, -2);
  result := AnsiString(lua_tostring(L, -1));
end;

function TLUA.GetValue(valName : AnsiString): Variant;
begin
  result := NULL;
  lua_pushstring(l, PAnsiChar(valName));
  lua_rawget(l, LUA_GLOBALSINDEX);
  try
    result := plua_tovariant(l, -1);
  finally
    lua_pop(l, 1);
  end;
end;

function TLUA.GetLuaCPath: AnsiString;
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'cpath');
  lua_rawget(L, -2);
  result := AnsiString(lua_tostring(L, -1));
end;

function TLUA.CallFunction(FunctionName: AnsiString;
  const Args: array of Variant; Results: PVariantArray = nil): Integer;
begin
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
  if @FOnException=@AValue then exit; // FD: 16/05/2010, changed AValue to @AValue
  FOnException:=AValue;
end;

procedure TLUA.SetOnLoadLibs(const AValue: TLuaOnLoadLibs);
begin
  if @FOnLoadLibs=@AValue then exit; // FD: 16/05/2010, changed AValue to @AValue
  FOnLoadLibs:=AValue;
  //if (L <> nil) and (FOnLoadLibs <> nil) then
  if (L <> nil) and (assigned(FOnLoadLibs)) then // FD: 16/05/2010, added assigned
    FOnLoadLibs(self);
end;

procedure TLUA.SetUseDebug(const AValue: Boolean);
begin
  if FUseDebug=AValue then exit;
  FUseDebug:=AValue;
end;

procedure TLUA.ErrorTest(errCode: Integer);
var
  msg : AnsiString;
begin
  if errCode <> 0 then
    begin
      msg := plua_tostring(l, -1);
      lua_pop(l, 1);
      HandleException(LuaException.Create(msg));
    end;
end;

procedure TLUA.HandleException(E: LuaException);
var
  title, msg : AnsiString;
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

procedure TLUA.SetValue(valName : AnsiString; const AValue: Variant);
begin
  if VarIsType(AValue, varString) then
    begin
      lua_pushliteral(l, PAnsiChar(valName));
      lua_pushstring(l, PAnsiChar(AnsiString(AValue)));
      lua_settable(L, LUA_GLOBALSINDEX);
    end
  else
    begin
      lua_pushliteral(l, PAnsiChar(valName));
      plua_pushvariant(l, AValue);
      lua_settable(L, LUA_GLOBALSINDEX);
    end;
end;

function TLUA.CallTableFunction(TableName, FunctionName: AnsiString;
  const Args: array of Variant; Results: PVariantArray): Integer;
var
  tblidx : integer;
begin
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
  FunctionName: AnsiString; out tblidx : Integer): Boolean;
begin
  lua_pushstring(L, PAnsiChar(TableName));
  lua_rawget(L, LUA_GLOBALSINDEX);
  result := lua_istable(L, -1);
  if result then
    begin
      tblidx := lua_gettop(L);
      lua_pushstring(L, PAnsiChar(FunctionName));
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

function TLUA.TableFunctionExists(TableName, FunctionName: AnsiString
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
  lua_getglobal(L, PAnsiChar(FThreadName));
  result := not lua_isnil(L, 1);
  lua_pop(L, 1);
end;

constructor TLUAThread.Create(LUAInstance: TLUA; ThreadName: AnsiString);
begin
  L := lua_newthread(LUAInstance.LuaState);
  FThreadName := ThreadName;
  lua_setglobal(LUAInstance.LuaState, PAnsiChar(ThreadName));
  FMaster := LUAInstance;
end;

destructor TLUAThread.Destroy;
begin
  lua_pushnil(FMaster.LuaState);
  lua_setglobal(FMaster.LuaState, PAnsiChar(FThreadName));
  inherited;
end;

function luaResume(L : PLua_State; NArgs:Integer; out Res : Integer) : Boolean;
begin
  Res := lua_resume(L, NArgs);
  result := Res <> 0;
end;

function TLUAThread.Start(TableName : AnsiString; AMethodName : AnsiString; const ArgNames: array of AnsiString; var ErrorString : AnsiString) : Boolean;
var
  i,
  rres : Integer;
begin
  FTableName := TableName;
  FMethodName := AMethodName;
  if TableName <> '' then
    begin
      lua_pushstring(L, PAnsiChar(TableName));
      lua_gettable(L, LUA_GLOBALSINDEX);
      plua_pushstring(L, PAnsiChar(AMethodName));
      lua_rawget(L, -2);
    end
  else
    lua_getglobal(L, PAnsiChar(AMethodName));

  for i := 0 to Length(ArgNames)-1 do
    lua_getglobal(L, PAnsiChar(ArgNames[i]));

  if luaResume(L, Length(ArgNames), rres) then
    begin
      ErrorString := lua_tostring(L, -1);
      result := false;
      exit;
    end
  else
    result := true;
end;

function TLUAThread.Resume(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : AnsiString) : Boolean;
var
  rres,
  i : Integer;
  msg : AnsiString;
begin
  lua_pushnumber(L, EllapsedTime);
  for i := 0 to Length(Args)-1 do
    plua_pushvariant(L, Args[i]);
  if luaResume(L, Length(Args)+1, rres) then
    begin
      ErrorString := lua_tostring(L, -1);
      msg := 'Error ('+IntToStr(rres)+'): '+ErrorString;
      result := false;
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
  var ErrorString: AnsiString);
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

function TLUAThreadList.SpinUp(TableName, AMethodName, ThreadName: AnsiString; var ErrorString : AnsiString) : Boolean;
var
  T : TLUAThread;
begin
  T := TLUAThread.Create(FLUAInstance, ThreadName);
  FThreads.Add(T);
  result := T.Start(TableName, AMethodName, [], ErrorString);
end;

function TLUAThreadList.IndexOf(ThreadName: AnsiString): Integer;
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
