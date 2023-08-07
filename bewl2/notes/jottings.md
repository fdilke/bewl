# Tagging mechanism, for type-safe equalizers

Do I have to retool whereTrue, so that instead of inventing a new type ("I") it tags the existing one?
Yes, and it's a fairly massive retool, so consider this carefully. Start with the equalizer. Instead of:

  final def doEqualizer[X: Dot, Y: Dot, RESULT](
    f: X ~> Y,
    f2: X ~> Y
  )(
    capture: [A] => Dot[A] ?=> Equalizer[A, X] => RESULT
  ): RESULT = ...

we hard-wire so that the new type A is always X Tagged TAG, where TAG is provided.
five-point plan to do this by:
1 introduce new API, doEqualizerNew where instead of A, you provide a TAG and a function that works on X Tagged TAG.
2 implement this in both our topoi.
3 fix up the built-in glue code in Topos to use this instead
4 get rid of the original API once it's no longer used
5 rename

