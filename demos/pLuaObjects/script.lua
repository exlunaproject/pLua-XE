btn = TButton:new()
btn.Caption = 'Click ME!'
btn.Left = 20
btn.Top = 20
function btn:OnClick ()
  ShowMessage('You clicked the button!')
end
btn.Visible = true
