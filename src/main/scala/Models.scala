import sangria.execution.deferred.HasId

object Models {

  case class Picture(width: Int, height: Int, url: Option[String])

  trait Identifiable {
    def id: Int
  }

  object Identifiable {
    implicit def hasId[T <: Identifiable]: HasId[T, Int] = HasId(_.id)
  }

  case class Product(id: Int, name: String, description: String, price: BigDecimal) extends Identifiable {
    def picture(size: Int): Picture =
      Picture(width = size, height = size, url = Some(s"http://fakeimg.pl/$size/?text=ID:%20$id"))
  }

  case class Category(id: String, name: String)

  object Category {
    implicit val hasId = HasId[Category, String](_.id)
  }

  case class Taxonomy(productId: Int, categoryId: String)

  case class Pen(id: Int, name: String) extends Identifiable
  case class Paper(id: Int, name: String) extends Identifiable
  case class Image(id: Int, name: String, penId: Int, paperId: Int) extends Identifiable

  case class Grouped(id: Int, name: String, category: Int) extends Identifiable

  //nested fetchers test
  case class Customer(id: Int, name: String, riskLevel: Int) extends Identifiable
  case class CustomerResult(id: Int, customerId: Int, riskLevelId: Int) extends Identifiable

  case class RiskLevel(id: Int, name: String) extends Identifiable

}
