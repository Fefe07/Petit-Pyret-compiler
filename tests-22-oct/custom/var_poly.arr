var x = raise("a")
fun f<A>(y :: A) -> A:
  x := y end 

f(3)
f("a")
