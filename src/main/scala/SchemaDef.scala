import sangria.execution.deferred._
import sangria.schema._

import scala.concurrent.Future

object SchemaDef {

  import Models._
  import sangria.macros.derive._

  def constantComplexity[Ctx](complexity: Double) =
    Some((_: Ctx, _: Args, child: Double) ⇒ child + complexity)

  val IdentifiableType = InterfaceType(
    "Identifiable",
    "Entity that can be identified",
    fields[Unit, Identifiable](
      Field("id", IntType, resolve = _.value.id)
    )
  )

  implicit val ProductType: ObjectType[Unit, Product] =
    deriveObjectType[Unit, Product](
      Interfaces(IdentifiableType),
      IncludeMethods("picture"), //by defaul macro cosinders fields only
      AddFields(
        Field("categories", ListType(CategoryType),
          complexity = constantComplexity(30),
          resolve = c => categoriesFetcher.deferRelSeq(category, c.value.id))
      )
    )

  implicit val PictureType: ObjectType[Unit, Picture] =
    deriveObjectType[Unit, Picture](
      ObjectTypeDescription("The product picture"),
      DocumentField("url", "Picture CDN URL")
    )

  /**
    * Category
    */

  implicit val CategoryType: ObjectType[Unit, Category] =
    deriveObjectType[Unit, Category](
      ObjectTypeDescription("The category of products"),
      AddFields(
        Field("products", ListType(ProductType),
          complexity = constantComplexity(30),
          resolve = c => productsFetcher.deferRelSeq(product, c.value.id))
      )
    )

  val product = Relation[Product, (Seq[String], Product), String]("product-category", _._1, _._2)
  val category = Relation[Category, (Seq[Int], Category), Int]("category-product", _._1, _._2)

  val productsFetcher: Fetcher[ShopRepository, Product, (Seq[String], Product), Int] = Fetcher.relCaching(
    (repo: ShopRepository, ids: Seq[Int]) => repo.products(ids),
    (repo: ShopRepository, ids: RelationIds[Product]) => repo.productsByCategories(ids(product))
  )
  val categoriesFetcher = Fetcher.relCaching(
    (repo: ShopRepository, ids: Seq[String]) => repo.categories(ids),
    (repo: ShopRepository, ids: RelationIds[Category]) => repo.categoriesByProducts(ids(category))
  )

  /**
    * PEN AND PAPER
    */

  val ImageByPen: Relation[Image, Image, Int] = Relation[Image, Int]("byPen", i => Seq(i.penId))
  val ImageByPaper: Relation[Image, Image, Int] = Relation[Image, Int]("byPaper", i => Seq(i.paperId))

  val imagesFetcher: Fetcher[ShopRepository, Image, Image, Int] = Fetcher.rel(
    (ctx: ShopRepository, ids: Seq[Int]) => ctx.images(ids),
    (ctx: ShopRepository, ids: RelationIds[Image]) => ctx.imagesByRelation(ids)
  )

  implicit val PenType: ObjectType[Unit, Pen] =
    deriveObjectType[Unit, Pen](
      ObjectTypeDescription("available pens"),
      AddFields(
        Field("images", ListType(ImageType),
          complexity = constantComplexity(1),
          resolve = c => imagesFetcher.deferRelSeq(ImageByPen, c.value.id))
      )
    )

  implicit val PaperType: ObjectType[Unit, Paper] =
    deriveObjectType[Unit, Paper](
      ObjectTypeDescription("available papers"),
      AddFields(
        Field("images", ListType(ImageType),
          complexity = constantComplexity(1),
          resolve = c => imagesFetcher.deferRelSeq(ImageByPaper, c.value.id))
      )
    )

  implicit val ImageType: ObjectType[Unit, Image] =
    deriveObjectType[Unit, Image](
      ObjectTypeDescription("available images"),
    )

  implicit val GroupedType: ObjectType[Unit, Grouped] =
    deriveObjectType[Unit, Grouped]()

  val groupedCategoryFetcher = Fetcher(
    (ctx: ShopRepository, categories: Seq[Int]) => ctx.groupedByCat(categories)
  )(HasId(_.category))


  //customer -> result -> risklevel
  val customersFetcher = Fetcher(
    (ctx: ShopRepository, ids: Seq[Int]) => ctx.customers(ids)
  )

  val risksLevelsFetcher = Fetcher(
    (ctx: ShopRepository, ids: Seq[Int]) => ctx.riskLevels(ids)
  )

  val CustomerResultRelation = Relation[CustomerResult, Int]("byCustomer", cr => Seq(cr.customerId))

  val customerResultsFetcher = Fetcher.relCaching(
    (ctx: ShopRepository, ids: Seq[Int]) => ctx.customerResults(ids),
    (ctx: ShopRepository, rids: RelationIds[CustomerResult]) => ctx.customerResultsByCustomerIds(rids(CustomerResultRelation))
  )


  implicit val RiskLevelType = deriveObjectType[Unit, RiskLevel]()

  implicit val CustomerResultType = deriveObjectType[Unit, CustomerResult](
    ReplaceField("riskLevelId", Field("riskLevel", RiskLevelType, resolve = ctx => risksLevelsFetcher.defer(ctx.value.riskLevelId))),
  )
  implicit val CustomerType = deriveObjectType[Unit, Customer](
    ReplaceField("riskLevel", Field("riskLevel", RiskLevelType, resolve = ctx => risksLevelsFetcher.defer(ctx.value.riskLevel))),
    AddFields(
      Field("results", ListType(CustomerResultType), resolve = ctx => customerResultsFetcher.deferRelSeq(CustomerResultRelation, ctx.value.id) )
    )
  )




  lazy val deferredResolver = DeferredResolver.fetchers(productsFetcher, categoriesFetcher, imagesFetcher,groupedCategoryFetcher, customersFetcher, risksLevelsFetcher, customerResultsFetcher)

  val IntID = Argument("id", IntType)

  val QueryType = ObjectType(
    "Query",
    fields[ShopRepository, Unit](
      Field("product", OptionType(ProductType),
        description = Some("Returns a product with specific `id`."),
        arguments = Argument("id", IntType) :: Nil,
        resolve = c => productsFetcher.defer(c.arg[Int]("id"))),

      Field("allProducts", ListType(ProductType),
        description = Some("Returns a list of all available products."),
        complexity = constantComplexity(100),
        resolve = _.ctx.allProducts
      ),
      Field("products", ListType(ProductType),
        description = Some("Returns a list of products for provided IDs."),
        arguments = Argument("ids", ListInputType(IntType)) :: Nil,
        resolve = c => productsFetcher.deferSeqOpt(c.arg[List[Int]]("ids"))
      ),
      Field("category", OptionType(CategoryType),
        description = Some("Returns a category with specific `id`."),
        arguments = Argument("id", StringType) :: Nil,
        resolve = c => categoriesFetcher.deferOpt(c.arg[String]("id"))),
      Field("categories", ListType(CategoryType),
        description = Some("Returns categories by provided ids"),
        complexity = constantComplexity(30),
        arguments = Argument("ids", ListInputType(StringType)) :: Nil,
        resolve = c => categoriesFetcher.deferSeqOpt(c.arg[List[String]]("ids"))
      ),
      Field("allCategories", ListType(CategoryType),
        description = Some("Returns a list of all available categories."),
        complexity = constantComplexity(250),
        resolve = _.ctx.allCategories
      ),
      Field("pens", ListType(PenType),
        description = Some("Returns a list of pens"),
        complexity = constantComplexity(1),
        resolve = _.ctx.allPens
      ),
      Field("papers", ListType(PaperType),
        description = Some("Returns a list of papers"),
        complexity = constantComplexity(1),
        resolve = _.ctx.allPapers
      ),
      Field("images", ListType(ImageType),
        description = Some("Returns a list of images"),
        complexity = constantComplexity(1),
        resolve = _ => imagesFetcher.deferSeq(List(1,2,3,4))
      ),
      Field("groups", ListType(GroupedType),
        resolve = _ => groupedCategoryFetcher.deferSeq(List(1,2)),
          description = Some("Returns a list of images"),
          complexity = constantComplexity(1)
      ),
      Field("customer", CustomerType,
        arguments = Argument("id", IntType) :: Nil,
        resolve = ctx => customersFetcher.defer(ctx arg IntID)
      ),
      Field("risks", ListType(RiskLevelType),
        arguments = Argument("ids", ListInputType(IntType)) :: Nil,
        resolve = ctx => risksLevelsFetcher.deferSeq(ctx.arg[List[Int]]("ids"))
      ),



    )
  )

  val ShopSchema = Schema(QueryType) //define entry point
}
