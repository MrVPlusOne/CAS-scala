import core.Symb
import core.operation.{Simplify, DiffInfo, Diff}

val diff=Diff(new DiffInfo("x"))
val symbs=List("a","b","c","d","e","x","y","z").map(Symb)
val List(a,b,c,d,e,x,y,z)=symbs

a*a*a