program Step13
var
  a : array[1..3] of integer;
begin
  a[1] := 10;
  a[2] := 20;
  a[3] := a[1] + a[2]
  writeln(a[3]);
end.
