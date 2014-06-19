r.AString = 'Test' -- r is created and registered in the application
r.Int = 123
r.Num = 1.23

r2 = MyRecord:new()
r2.AString = 'Test2'
r2.Int = 345
r2.Num = 6.78

function serialize(o, pre)
  if pre == nil then
    pre = ''
  end
  if type(o)~='table' then
    if type(o) == 'string' then
      return '"' .. o .. '"'
    elseif type(o) ~= 'function' then
      return o
    else
      return 'function () end'
    end
  else
    local res = '{\n'
    for k, v in pairs(o) do
      res = res .. pre .. "  ['" .. tostring(k) .. "']=" .. serialize(v, pre..'  ') .. ',\n'
    end
    res = res .. pre .. '}'
    return res
  end
end

ShowMessage('MyRecord='..serialize(MyRecord)..'\nr.AString='..r.AString..'\nr.Int='..r.Int..'\nr.Num='..r.Num..'\nr2.AString='..r2.AString..'\nr2.Int='..r2.Int..'\nr2.Num='..r2.Num)
r2:release()