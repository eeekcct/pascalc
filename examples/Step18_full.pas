program Step19;

function Fib(n : integer) : integer;
begin
  if n <= 1 then
    Fib := n
  else
    Fib := Fib(n - 1) + Fib(n - 2);
end;

var
  i : integer;
begin
  for i := 0 to 7 do
    writeln(Fib(i));
end.
