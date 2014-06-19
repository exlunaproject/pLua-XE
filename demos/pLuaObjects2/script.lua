-- Change the caption of the existing button
btn.Caption = 'Click ME!'
-- Set its position
btn.Left = 20
btn.Top = 20
-- Add an event handler
function btn:OnClick ()
  ShowMessage('You clicked the button!')
end
-- Make it visible
btn.Visible = true
