package com.fdilke.bewl.helper

object Memoize {
  def apply[INPUT, OUTPUT](
    function: INPUT => OUTPUT
  ) =
    new MemoizedFunction[INPUT, OUTPUT](
      function
    )

  class MemoizedFunction[INPUT, OUTPUT](
    function: INPUT => OUTPUT
  ) extends (INPUT => OUTPUT) {
    private val resultMap =
      scala.collection.mutable.Map[INPUT, OUTPUT]()

    def apply(input: INPUT): OUTPUT =
      resultMap.getOrElseUpdate(
        input,
        function(input)
      )
  }

  object generic {
    def apply[INPUT[T], OUTPUT[T]](
      function: INPUT[Nothing] => OUTPUT[Nothing]
    ) =
      new MemoizedFunctionGeneric[INPUT, OUTPUT, Any](
        function.asInstanceOf[
          INPUT[_] => OUTPUT[_]
        ]
      )

    class MemoizedFunctionGeneric[
      INPUT[T <: BASE],
      OUTPUT[T <: BASE],
      BASE
    ](
      function: INPUT[_ <: BASE] => OUTPUT[_ <: BASE]
    ) {
      private val resultMap =
        scala.collection.mutable.Map[
          INPUT[_],
          OUTPUT[_]
        ]()

      def apply[T <: BASE](input: INPUT[T]): OUTPUT[T] =
        resultMap
          .getOrElseUpdate(
            input,
            function(input)
          )
          .asInstanceOf[OUTPUT[T]]
    }

    def withLowerBound[
      INPUT[T <: BASE],
      OUTPUT[T <: BASE],
      BASE
    ](
      function: INPUT[BASE] => OUTPUT[BASE]
    ) =
      new MemoizedFunctionGeneric[INPUT, OUTPUT, BASE](
        function.asInstanceOf[
          INPUT[_ <: BASE] => OUTPUT[_ <: BASE]
        ]
      )
  }
}
