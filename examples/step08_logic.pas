program Step08;
var
  a : boolean;
  b : boolean;
begin
  a := true;
  b := false;

  if a and not b then
    writeln(1)
  else
    writeln(0);

  if a or b then
    writeln(2);
end.
