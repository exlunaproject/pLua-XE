unit LuaButton;

interface

uses
  Classes, SysUtils, lua, pLuaObject, plua, Buttons, StdCtrls;

procedure RegisterLuaButton(L : Plua_State);
procedure RegisterExistingButton(L : Plua_State; InstanceName : AnsiString; Instance : TButton);

implementation

uses
  MainForm;

type

  { TButtonDelegate }

  TButtonDelegate = class(TLuaObjectEventDelegate)
  public
    constructor Create(InstanceInfo : PLuaInstanceInfo; obj : TObject); override;
    destructor Destroy; override;
    
    procedure ClickHandler(Sender : TObject);
  end;

{ TButtonDelegate }

constructor TButtonDelegate.Create(InstanceInfo: PLuaInstanceInfo; obj : TObject);
begin
  inherited Create(InstanceInfo, obj);
  TButton(obj).OnClick := ClickHandler;
end;

destructor TButtonDelegate.Destroy;
begin
  TButton(fobj).OnClick := nil;
  inherited Destroy;
end;

procedure TButtonDelegate.ClickHandler(Sender: TObject);
begin
  CallEvent('OnClick');
end;

var
  ButtonInfo : TLuaClassInfo;

function newButton(l : PLua_State; paramidxstart, paramcount : Integer; InstanceInfo : PLuaInstanceInfo) : TObject;
begin
  result := TButton.Create(frmMain);
  TButton(Result).Parent := frmMain;
  TButton(Result).Visible := true;
  TButtonDelegate.Create(InstanceInfo, result);
end;


function GetCaption(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  lua_pushstring(l, btn.Caption);
  result := 1;
end;

function SetCaption(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  btn.Caption := lua_tostring(l, paramidxstart);
  result := 0;
end;

function GetLeft(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  lua_pushinteger(l, btn.Left);
  result := 1;
end;

function SetLeft(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  btn.Left := lua_tointeger(l, paramidxstart);
  result := 0;
end;

function GetTop(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  lua_pushinteger(l, btn.Top);
  result := 1;
end;

function SetTop(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  btn.Top := lua_tointeger(l, paramidxstart);
  result := 0;
end;

function Click(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  result := 0;
  btn := TButton(target);
  plua_CallObjectEvent(plua_GetObjectInfo(l, btn), 'OnClick', []);
end;

function GetHeight(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  lua_pushinteger(l, btn.Height);
  result := 1;
end;

function SetHeight(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  btn.Height := lua_tointeger(l, paramidxstart);
  result := 0;
end;

function GetWidth(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  lua_pushinteger(l, btn.Width);
  result := 1;
end;

function SetWidth(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  btn.Width := lua_tointeger(l, paramidxstart);
  result := 0;
end;

function GetVisible(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  lua_pushboolean(l, btn.Visible);
  result := 1;
end;

function SetVisible(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  btn.Visible := lua_toboolean(l, paramidxstart);
  result := 0;
end;

function GetEnabled(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  lua_pushboolean(l, btn.Enabled);
  result := 1;
end;

function SetEnabled(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  btn : TButton;
begin
  btn := TButton(target);
  btn.Enabled := lua_toboolean(l, paramidxstart);
  result := 0;
end;

procedure RegisterLuaButton(L: Plua_State);
begin
  plua_registerclass(L, ButtonInfo);
end;

procedure RegisterExistingButton(L: Plua_State; InstanceName : AnsiString; Instance: TButton);
begin
  TButtonDelegate.Create(plua_registerExisting(L, InstanceName, Instance, @ButtonInfo), Instance);
end;

function setButtonInfo : TLuaClassInfo;
begin
  plua_initClassInfo(result);
  result.ClassName := 'TButton';
  result.New := @newButton;
  plua_AddClassProperty(result, 'Caption', @GetCaption, @SetCaption);
  plua_AddClassProperty(result, 'Left', @GetLeft, @SetLeft);
  plua_AddClassProperty(result, 'Top', @GetTop, @SetTop);
  plua_AddClassProperty(result, 'Width', @GetWidth, @SetWidth);
  plua_AddClassProperty(result, 'Height', @GetHeight, @SetHeight);
  plua_AddClassProperty(result, 'Visible', @GetVisible, @SetVisible);
  plua_AddClassProperty(result, 'Enabled', @GetEnabled, @SetEnabled);
  plua_AddClassMethod(result, 'Click', @Click);
end;

initialization
  ButtonInfo := setButtonInfo;

finalization

end.

