2  + 3
List(2,3,4,5).length
object Widget {
  println("Greetings from inside the widget")
  for(x <- List(1,2,3) ;
      y <- Seq('a, 'b, 'c))
    yield (x + y.hashCode())
}