unit RecTest;

interface

uses
  Classes, SysUtils, pLuaRecord, lua, plua;

type
  PMyRecord = ^TMyRecord;
  TMyRecord = record
    AString : AnsiString;
    Int     : Integer;
    Num     : Double;
  end;

procedure RegisterMyRecordType( L : Plua_State );
procedure RegisterExistingMyRecord( L : Plua_State; InstanceName : AnsiString; RecordPointer : Pointer);

var
  MyRecordInfo : TLuaRecordInfo;

implementation

function getAString(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  plua_pushstring(L, PMyRecord(RecordPointer)^.AString);
  result := 1;
end;

function setAString(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  PMyRecord(RecordPointer)^.AString := plua_tostring(l, paramidxstart);
  result := 0;
end;

function getInt(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  lua_pushinteger(L, PMyRecord(RecordPointer)^.Int);
  result := 1;
end;

function setInt(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  PMyRecord(RecordPointer)^.Int := lua_tointeger(l, paramidxstart);
  result := 0;
end;

function getnumber(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  lua_pushnumber(L, PMyRecord(RecordPointer)^.Num);
  result := 1;
end;

function setnumber(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  PMyRecord(RecordPointer)^.Num := lua_tonumber(l, paramidxstart);
  result := 0;
end;

function newMyRecord(l : Plua_State; paramidxstart, paramcount : integer; InstanceInfo : PLuaRecordInstanceInfo) : Pointer;
var
  r : PMyRecord;
begin
  new(r);
  result := r;
end;

procedure disposeMyRecord( RecordPointer : pointer; l : Plua_State );
begin
  Freemem(PMyRecord(RecordPointer));
end;

function MyProcInfo : TLuaRecordInfo;
begin
  plua_initRecordInfo(Result);
  result.RecordName := 'MyRecord';
  result.New := @newMyRecord;
  result.Release := @disposeMyRecord;
  plua_AddRecordProperty(result, 'AString', @getAString, @setAString);
  plua_AddRecordProperty(result, 'Int', @getInt, @setInt);
  plua_AddRecordProperty(result, 'Num', @getnumber, @setnumber);
end;

procedure RegisterMyRecordType(L: Plua_State);
begin
  plua_registerRecordType(l, MyRecordInfo);
end;

procedure RegisterExistingMyRecord(L: Plua_State; InstanceName: AnsiString;
  RecordPointer: Pointer);
begin
  plua_registerExistingRecord(L, InstanceName, RecordPointer, @MyRecordInfo);
end;

initialization
  MyRecordInfo := MyProcInfo;

finalization

end.

