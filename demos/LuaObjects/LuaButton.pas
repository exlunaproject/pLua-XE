unit LuaButton;

interface

uses
  Classes, SysUtils, lua, LuaObject, Buttons, StdCtrls;

type
  { TLuaButton }
  TLuaButton = class(TLuaObject)
  private
    procedure ButtonClickEvent(Sender: TObject);
  public
    btn : TButton;
    procedure CommonCreate(LuaState : PLua_State; AParent : TLuaObject = nil); override;
    function  GetPropValue(propName : String): Variant; override;
    function  SetPropValue(PropName : String; const AValue: Variant) : Boolean; override;
    destructor Destroy; override;
    procedure Click;
  end;

procedure RegisterLuaButton(L : Plua_State);

implementation

uses
  MainForm;

function Button_Click(l : PLua_State) : Integer; cdecl;
var
  btn : TLuaButton;
begin
  result := 0;
  if (lua_gettop(l) < 1) then
    exit;
  btn := TLuaButton(LuaToTLuaObject(l, 1));
  btn.Click;
end;

function Button_new(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
begin
  result := TLuaButton.Create(L, AParent);
end;

function new_Button(L : PLua_State) : Integer; cdecl;
var
  p : TLuaObjectNewCallback;
begin
  p := @Button_new;
  result := new_LuaObject(L, 'TButton', p);
end;

procedure methods_Button(L : Plua_State; classTable : Integer);
begin
  RegisterMethod(L, 'Click', @Button_Click, classTable);
end;

procedure RegisterLuaButton(L: Plua_State);
begin
  RegisterTLuaObject(L, 'TButton', @new_Button, @methods_Button);
end;

{ TLuaButton }

procedure TLuaButton.CommonCreate(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited CommonCreate(LuaState, AParent);
  btn := TButton.Create(frmMain);
  btn.OnClick := ButtonClickEvent;
  btn.Parent := frmMain;
end;

procedure TLuaButton.ButtonClickEvent(Sender: TObject);
begin
  CallEvent('OnClick');
end;

function TLuaButton.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'Caption') = 0 then
    result := btn.Caption
  else if CompareText(propName, 'Width') = 0 then
    result := btn.Width
  else if CompareText(propName, 'Height') = 0 then
    result := btn.Height
  else if CompareText(propName, 'Left') = 0 then
    result := btn.Left
  else if CompareText(propName, 'Top') = 0 then
    result := btn.Top
  else if CompareText(propName, 'Visible') = 0 then
    result := btn.Visible
  else if CompareText(propName, 'Enabled') = 0 then
    result := btn.Enabled
  else
    Result:=inherited GetPropValue(propName);
end;

function TLuaButton.SetPropValue(PropName: String; const AValue: Variant
  ): Boolean;
begin
  result := true;
  if CompareText(propName, 'Caption') = 0 then
    btn.Caption := AValue
  else if CompareText(propName, 'Width') = 0 then
    btn.Width := AValue
  else if CompareText(propName, 'Height') = 0 then
    btn.Height := AValue
  else if CompareText(propName, 'Left') = 0 then
    btn.Left := AValue
  else if CompareText(propName, 'Top') = 0 then
    btn.Top := AValue
  else if CompareText(propName, 'Visible') = 0 then
    btn.Visible := AValue
  else if CompareText(propName, 'Enabled') = 0 then
    btn.Enabled := AValue
  else
    Result:=inherited SetPropValue(propName, AValue);
end;

destructor TLuaButton.Destroy;
begin
  inherited Destroy;
end;

procedure TLuaButton.Click;
begin
  btn.Click;
end;

end.

