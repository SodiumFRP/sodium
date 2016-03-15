namespace Sodium

type 'T Cell private(stream : 'T Stream, value : 'T, usingInitialValue : bool, setupListener : bool) =
    internal new(value : 'T) =
        new Cell<'T>(new Stream<'T>(), value, false, false)
    internal new(stream : 'T Stream, initialValue : 'T) =
        new Cell<'T>(stream, initialValue, true, true)