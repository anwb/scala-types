object phantom {
  sealed trait State
  final class Start extends State
  final class Step1 extends State
  final class Step2 extends State
  final class Stop  extends State

  trait Request
  case class Req1(s: String) extends Request
  case class Req2(s: String) extends Request
  case class Req3(s: String) extends Request

  trait Response
  case class Res1(s: String) extends Response
  case class Res2(s: String) extends Response
  case class Res3(s: String) extends Response

  class CC[S <: State] {
    def handle[T >: S <: Start](m: Req1) = (Res1(m.s), advance[Step1])
    def handle[T >: S <: Step1](m: Req2) = (Res2(m.s), advance[Step2])
    def handle[T >: S <: Step2](m: Req3) = (Res3(m.s), advance[Stop])
    private def advance[T <: State] = this.asInstanceOf[CC[T]]
  }

  object CC {
    def create() = new CC[Start]
  }
}

object Main {
  import phantom._
  def run() = {
    val cc0 = CC.create()
    val (res1, cc1) = cc0.handle(Req1("1st things 1st"))
    val (res2, cc2) = cc1.handle(Req2("2nd things 2nd"))
    val (res3, cc3) = cc2.handle(Req3("3rd things 3rd"))

    // will not compile
    // cc0.handle(Req2("boom!"))
    // cc0.handle(Req3("boom!"))

    // cc1.handle(Req1("boom!"))
    // cc1.handle(Req3("boom!"))

    // cc2.handle(Req1("boom!"))
    // cc2.handle(Req2("boom!"))

    // cc3.handle(Req1("boom!"))
    // cc3.handle(Req2("boom!"))
    // cc3.handle(Req3("boom!"))
  }
}