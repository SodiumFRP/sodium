protocol IMaybe {
    func Match(hasValueAction: (Self)->Void, nothingAction: Action)
    func Match<TResult>(hasValueFunc: (Self) -> TResult, nothingFunc: () -> TResult) -> TResult
}
