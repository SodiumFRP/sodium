package sodium

trait Handler[A] {
  def run(a: A)
}

