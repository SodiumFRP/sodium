package sodium

trait TransactionHandler[A] {
  def run(trans: Transaction, a: A)
}