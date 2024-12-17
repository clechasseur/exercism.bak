def nextn:
  if . % 2 == 0
  then . / 2
  else . * 3 + 1
  end
;

def collatz($steps):
  if . == 1
  then $steps
  else nextn | collatz($steps + 1)
  end
;

def steps:
  if . <= 0
  then "Only positive integers are allowed" | halt_error
  else collatz(0)
  end
;
