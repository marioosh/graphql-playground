import Models._
import sangria.execution.deferred.{RelationIds, SimpleRelation}
import slick.jdbc.H2Profile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class ShopRepository(db: Database) {

  import ShopRepository._

  def allProducts = db.run(Products.result)

  def products(ids: Seq[Int]): Future[Seq[Product]] = db.run(Products.filter(_.id inSet ids).result)

  def allCategories = db.run(Categories.result)

  def categories(ids: Seq[String]): Future[Seq[Category]] = db.run(Categories.filter(_.id inSet ids).result)

  def productsByCategories(categoriesIds: Seq[String]): Future[Seq[(Seq[String], Product)]] =
    db.run(
      Taxonometry
        .filter(_.categoryId inSet categoriesIds)
        .join(Products).on(_.productId === _.id)
        .result)
      .map { result =>
        result.groupBy(_._2.id).toVector.map {
          case (_, products) ⇒ products.map(_._1.categoryId) → products.head._2
        }
      }

  def categoriesByProducts(productsIds: Seq[Int]): Future[Seq[(Seq[Int], Category)]] =
    db.run(
      Taxonometry
        .filter(_.productId inSet productsIds)
        .join(Categories).on(_.categoryId === _.id)
        .result)
      .map { result =>
        result.groupBy(_._2.id).toVector.map {
          case (_, categories) ⇒ categories.map(_._1.productId) → categories.head._2
        }
      }


  def pens(ids: Seq[Int]) = db.run(Pens filter (_.id inSet ids) result)

  def allPens = db.run(Pens.result)

  def papers(ids: Seq[Int]) = db.run(Papers filter (_.id inSet ids) result)

  def allPapers = db.run(Papers.result)

  def images(ids: Seq[Int]) ={
    println(s"Images: ${ids.mkString}")
    db.run(Images filter (_.id inSet ids) result)
  }



  def imagesByRelation(rel: RelationIds[Image]) = {
    db.run(
      Images.filter { image =>
       rel.rawIds.collect({
          case (SimpleRelation("byPaper"), ids: Seq[Int]) => image.paperId inSet ids
          case (SimpleRelation("byPen"), ids: Seq[Int]) => image.penId inSet ids
        }).reduceLeftOption(_ || _).getOrElse(true: Rep[Boolean])
      } result)
  }

  def groupedByCat(ids: Seq[Int]): Future[Seq[Grouped]] = {
    db.run(
      Groupeds.filter (_.category inSet ids) result
    )
  }

  def customers(ids: Seq[Int]) = {
    println(s"fetching for customers: ${ids.mkString(",")}")
    db.run(
      Customers.filter (_.id inSet ids) result
    )
  }

  def riskLevels(ids: Seq[Int]) = {
    println(s"fetching for risk levels: ${ids.mkString(",")}")
    db.run(
      RiskLevels.filter (_.id inSet ids) result
    )
  }

  def customerResults(ids: Seq[Int]) = {
    println(s"fetching for customer results: ${ids.mkString(",")}")
    db.run(
      CustomerResults.filter (_.id inSet ids) result
    )
  }

  def customerResultsByCustomerIds(ids: Seq[Int]) = {
    println(s"fetching for customer results for customers: ${ids.mkString(",")}")
    db.run(
      CustomerResults.filter (_.customerId inSet ids) result
    )
  }

  def close() = db.close()
}

object ShopRepository {

  class ProductTable(tag: Tag) extends Table[Product](tag, "PRODUCTS") {
    def id = column[Int]("PRODUCT_ID", O.PrimaryKey)

    def name = column[String]("NAME")

    def description = column[String]("DESCRIPTION")

    def price = column[BigDecimal]("PRICE")

    def * = (id, name, description, price) <> ((Product.apply _).tupled, Product.unapply)
  }

  val Products = TableQuery[ProductTable]

  class CategoryTable(tag: Tag) extends Table[Category](tag, "CATEGORY") {
    def id = column[String]("CATEGORY_ID", O.PrimaryKey)

    def name = column[String]("NAME")

    def * = (id, name) <> ((Category.apply _).tupled, Category.unapply)
  }

  val Categories = TableQuery[CategoryTable]


  /**
    * JOIN TABLE
    */
  class TaxonomyTable(tag: Tag) extends Table[Taxonomy](tag, "PRODUCT_CATEGORY") {
    def productId = column[Int]("PRODUCT_ID")

    def categoryId = column[String]("CATEGORY_ID")

    //relations
    def product = foreignKey("PRODUCT_FK", productId, Products)(_.id)

    def category = foreignKey("CATEGORY_FK", categoryId, Categories)(_.id)

    def idx = index("UNIQUE_IDX", (productId, categoryId), unique = true)

    def * = (productId, categoryId) <> ((Taxonomy.apply _).tupled, Taxonomy.unapply)
  }

  val Taxonometry = TableQuery[TaxonomyTable]

  class PenTable(tag: Tag) extends Table[Pen](tag, "PENS"){
    def id = column[Int]("PEN_ID", O.PrimaryKey)

    def name = column[String]("NAME")

    def * = (id, name) <> ((Pen.apply _).tupled, Pen.unapply)
  }

  val Pens = TableQuery[PenTable]

  class PaperTable(tag: Tag) extends Table[Paper](tag, "PAPERS"){
    def id = column[Int]("PAPER_ID", O.PrimaryKey)

    def name = column[String]("NAME")

    def * = (id, name) <> ((Paper.apply _).tupled, Paper.unapply)
  }

  val Papers = TableQuery[PaperTable]

  class ImageTable(tag: Tag) extends Table[Image](tag, "IMAGES"){
    def id = column[Int]("IMAGE_ID", O.PrimaryKey)

    def name = column[String]("NAME")

    def penId = column[Int]("PEN_ID")
    def paperId = column[Int]("PAPER_ID")

    def pen = foreignKey("PEN_FK", penId, Pens)(_.id)
    def paper = foreignKey("PAPER_FK", paperId, Papers)(_.id)

    def * = (id, name, penId, paperId) <> ((Image.apply _).tupled, Image.unapply)
  }

  val Images = TableQuery[ImageTable]

  class GroupedTable(tag: Tag) extends Table[Grouped](tag, "GROUPED"){
    def id = column[Int]("ID", O.PrimaryKey)

    def name = column[String]("NAME")
    def category = column[Int]("CAT")

    def * = (id, name, category) <> ((Grouped.apply _).tupled, Grouped.unapply)
  }

  val Groupeds = TableQuery[GroupedTable]

  class RiskLevelTable(tag: Tag) extends Table[RiskLevel](tag, "RISK_LEVEL"){
    def id = column[Int]("ID", O.PrimaryKey)
    def name = column[String]("NAME")

    def * = (id, name) <> ((RiskLevel.apply _).tupled, RiskLevel.unapply)
  }

  val RiskLevels = TableQuery[RiskLevelTable]

  class CustomerTable(tag: Tag) extends Table[Customer](tag, "CUSTOMERS"){
    def id = column[Int]("ID", O.PrimaryKey)

    def name = column[String]("NAME")
    def riskLevelId = column[Int]("RISK_LEVEL")

    def rl_fk = foreignKey("C_RL_FK", riskLevelId, RiskLevels)(_.id)

    def * = (id, name, riskLevelId) <> ((Customer.apply _).tupled, Customer.unapply)
  }

  val Customers = TableQuery[CustomerTable]

  class CustomerResultTable(tag: Tag) extends Table[CustomerResult](tag, "CUSTOMER_RESULTS"){
    def id = column[Int]("ID", O.PrimaryKey)

    def customerId = column[Int]("CUSTOMER_ID")
    def riskLevelId = column[Int]("RISK_LEVEL")

    def rl_fk = foreignKey("CR_RL_FK", riskLevelId, RiskLevels)(_.id)
    def c_fk = foreignKey("CR_C_FK", customerId, Customers)(_.id)

    def * = (id, customerId, riskLevelId) <> ((CustomerResult.apply _).tupled, CustomerResult.unapply)
  }

  val CustomerResults = TableQuery[CustomerResultTable]

  val databaseSetup = DBIO.seq(
    (Products.schema ++
      Categories.schema ++
      Taxonometry.schema ++
      Pens.schema ++
      Papers.schema ++
      Images.schema ++
      Groupeds.schema ++
      RiskLevels.schema ++
      CustomerResults.schema ++
      Customers.schema
      ).create,

    Products ++= Seq(
      Product(1, "Cheescake", "Tasty", BigDecimal(12.34)),
      Product(2, "Health Potion", "+50 HP", BigDecimal(98.89)),
      Product(3, "Pineapple", "The biggest one", BigDecimal(0.99)),
      Product(4, "Bull's egg", "The left one", BigDecimal(100.99)),
      Product(5, "Water", "Bottled", BigDecimal(0.25)),
      Product(6, "Candle", "", BigDecimal(13.99))
    ),
    Categories ++= Seq(
      Category("1", "Food"),
      Category("2", "Magic ingredients"),
      Category("3", "Home interior")
    ),
    Taxonometry ++= Seq(
      Taxonomy(1, "1"),
      Taxonomy(2, "2"),
      Taxonomy(3, "1"),
      Taxonomy(4, "1"),
      Taxonomy(4, "2"),
      Taxonomy(5, "1"),
      Taxonomy(5, "2"),
      Taxonomy(6, "3"),
      Taxonomy(6, "2")
    ),

    Pens ++= Seq(
      Pen(1, "Blue"),
      Pen(2, "Red")
    ),

    Papers ++= Seq(
      Paper(1, "thick"),
      Paper(2, "thin")
    ),

    Images ++= Seq(
      Image(1, "blue-thick",1,1),
      Image(2, "blue-thin",1,2),
      Image(3, "red-thick",2,1),
      Image(4, "red-thin",2,2)
    ),

    Groupeds ++= Seq(
      Grouped(1, "Gr 1 1", 1),
      Grouped(2, "Gr 2 2", 2),
      Grouped(3, "Gr 3 2", 2),
      Grouped(4, "Gr 4 3", 3),
      Grouped(5, "Gr 5 3", 3),
      Grouped(6, "Gr 6 3", 3)
    ),


    RiskLevels ++= Seq(
      RiskLevel(1, "Low"),
      RiskLevel(2, "Medium"),
      RiskLevel(3, "High")
    ),


    Customers ++= Seq(
      Customer(1, "CustLow", 1),
      Customer(2, "CustMed", 2),
      Customer(3, "CustHigh", 3)
    ),

    CustomerResults ++= Seq(
      CustomerResult(1, 1, 1),
      CustomerResult(2, 1, 2),
      CustomerResult(3, 2, 1),
      CustomerResult(4, 2, 2),
      CustomerResult(5, 2, 3)

    )

  )

  def createDatabase() = {
    val db = Database.forConfig("h2mem")

    Await.result(db.run(databaseSetup), 10 seconds)

    new ShopRepository(db)
  }

}
