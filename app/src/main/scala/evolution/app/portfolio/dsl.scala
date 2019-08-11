// <<<<<<< Updated upstream
// // package evolution.app.portfolio

// // import com.github.ghik.silencer.silent
// // import evolution.app.codec.JsonCodec
// // import evolution.app.codec.config.DrawingJsonCodec
// // import evolution.app.model.context.DrawingContext
// // import evolution.app.model.definition.DrawingDefinition
// // import evolution.app.model.state.DrawingState
// // import evolution.app.react.component.config.ConfigComponent
// // import evolution.compiler.phases.compiling.model.VarContext
// // import evolution.compiler.types.Type
// // import evolution.data
// // import evolution.data.Expr
// // import evolution.geometry.Point
// // import evolution.compiler.phases.All
// // import japgolly.scalajs.react.{ Callback, PropsChildren }
// // import japgolly.scalajs.react.component.Scala.BackendScope
// // import japgolly.scalajs.react.extra.StateSnapshot
// // import japgolly.scalajs.react.vdom.html_<^._
// // import evolution.compiler.types.TypeClasses._
// // import evolution.compiler.types.TypeBinding
// // import evolution.materialization.Evolution
// // import evolution.compiler.types.TypeBindings
// // import evolution.compiler.phases.typing.config.TypingConfig
// // import evolution.app.react.component.presentational.Editor

// // // This is a big epic mess
// // object dsl extends DrawingDefinition {

// //   val name = "drawing dsl"

// //   private val predefinedVars = List("left", "bottom", "right", "top")
// //   private val initialVarContext = new VarContext(predefinedVars)
// //   private val predefinedVarsTypeBindings = new TypeBindings(
// //     Map(
// //       "left" -> TypeBinding.Variable("left", Qualified(Type.Dbl)),
// //       "right" -> TypeBinding.Variable("right", Qualified(Type.Dbl)),
// //       "top" -> TypeBinding.Variable("top", Qualified(Type.Dbl)),
// //       "bottom" -> TypeBinding.Variable("bottom", Qualified(Type.Dbl))
// //     )
// //   )

// //   // TODO I would really like to move expr into the state, but that cannot be done at the moment because
// //   // materialize method needs to render the stream just using the Config. So the Expr HAS to go inside the config.
// //   case class Config(serialisedExpr: String, expr: Option[Expr[Evolution[Point]]])

// //   object Config {
// //     def from(serialisedExpr: String): (Config, State) = {
// //       val eitherExprOrError =
// //         All
// //           .compile(
// //             serialisedExpr,
// //             Type.Evo(Type.Point),
// //             // TODO here we shouldn't know about constants
// //             TypingConfig.constantQualifiedTypes.merge(predefinedVarsTypeBindings),
// //             initialVarContext
// //           )
// //           .map(_.asInstanceOf[Expr[Evolution[Point]]])

// //       (Config(serialisedExpr, eitherExprOrError.toOption), State(eitherExprOrError.swap.toOption))
// //     }
// //   }

// //   case class State(message: Option[String])

// //   class Backend(bs: BackendScope[StateSnapshot[Config], State]) {
// //     def render(snapshot: StateSnapshot[Config], state: State, @silent children: PropsChildren): VdomElement = {
// //       val stringSnapshot = StateSnapshot[String](snapshot.value.serialisedExpr) {
// //         case (Some(serialized), _) =>
// //           if (serialized == snapshot.value.serialisedExpr) Callback.empty
// //           else {
// //             val (config, state) = Config.from(serialized)
// //             snapshot.setState(config) >> bs.setState(state)
// //           }
// //         case (None, cb) => cb
// //       }

// //       Editor.component(Editor.Props(stringSnapshot, state.message))
// //     }
// //   }

// //   override val configComponent: ConfigComponent[Config] =
// //     japgolly.scalajs.react.ScalaComponent
// //       .builder[StateSnapshot[Config]]("DSL Config")
// //       // A bit hacky here, we re-parse the expression to load the error message. See TODO above
// //       .initialStateFromProps(props => Config.from(props.value.serialisedExpr)._2)
// //       .renderBackendWithChildren[Backend]
// //       .build

// //   override def materialize(ctx: DrawingContext, state: DrawingState): Iterator[Point] = state.config.expr.map { expr =>
// //     data.EvaluationModule.materializeExpr(state.seed, bindPredefinedVars(ctx, expr))
// //   }.getOrElse(Iterator.empty)

// //   private def bindPredefinedVars(ctx: DrawingContext, expr: Expr[Evolution[Point]]): Expr[Evolution[Point]] = {
// //     import Expr._
// //     Let[Double, Evolution[Point]](
// //       "top",
// //       Dbl(ctx.top),
// //       Let[Double, Evolution[Point]](
// //         "right",
// //         Dbl(ctx.right),
// //         Let[Double, Evolution[Point]](
// //           "bottom",
// //           Dbl(ctx.bottom),
// //           Let[Double, Evolution[Point]]("left", Dbl(ctx.left), expr)
// //         )
// //       )
// //     )
// //   }

// //   val initialConfig: Config = Config.from("integrate(point(0, 0), @point(uniform(-2, 2), uniform(-2, 2)))")._1

// //   override def configCodec: JsonCodec[Config] =
// //     DrawingJsonCodec
// // }
// =======
// package evolution.app.portfolio

// import com.github.ghik.silencer.silent
// import evolution.app.codec.JsonCodec
// import evolution.app.codec.config.DrawingJsonCodec
// import evolution.app.model.context.DrawingContext
// import evolution.app.model.definition.DrawingDefinition
// import evolution.app.model.state.DrawingState
// import evolution.app.react.component.config.{ ConfigComponent, instances }
// import evolution.compiler.phases.compiling.model.VarContext
// import evolution.compiler.types.Type
// import evolution.data
// import evolution.data.Expr
// import evolution.geometry.Point
// import evolution.compiler.phases.All
// import japgolly.scalajs.react.{ Callback, PropsChildren }
// import japgolly.scalajs.react.component.Scala.BackendScope
// import japgolly.scalajs.react.extra.StateSnapshot
// import japgolly.scalajs.react.vdom.html_<^._
// import evolution.compiler.types.TypeClasses._
// import evolution.compiler.types.TypeBinding
// import evolution.materialization.Evolution
// import evolution.compiler.types.TypeBindings
// import evolution.compiler.phases.typing.config.TypingConfig
// import evolution.compiler.ast.AST

// // This is a big epic mess
// object dsl extends DrawingDefinition[Point] {

//   val name = "drawing dsl"

//   private val predefinedVars = List("left", "bottom", "right", "top")
//   private val initialVarContext = new VarContext(predefinedVars)
//   private val predefinedVarsTypeBindings = new TypeBindings(
//     Map(
//       "left" -> TypeBinding.Variable("left", Qualified(Type.Dbl)),
//       "right" -> TypeBinding.Variable("right", Qualified(Type.Dbl)),
//       "top" -> TypeBinding.Variable("top", Qualified(Type.Dbl)),
//       "bottom" -> TypeBinding.Variable("bottom", Qualified(Type.Dbl))
//     )
//   )

//   // TODO I would really like to move expr into the state, but that cannot be done at the moment because
//   // stream method needs to render the stream just using the Config. So the Expr HAS to go inside the config.
//   case class Config(serialisedExpr: String, expr: Option[Expr[Evolution[Point]]])

//   object Config {
//     def from(serialisedExpr: String): (Config, State) = {
//       val eitherExprOrError =
//         All
//           .compile(
//             serialisedExpr,
//             Type.Evo(Type.Point),
//             // TODO here we shouldn't know about constants
//             TypingConfig.constantQualifiedTypes.merge(predefinedVarsTypeBindings),
//             initialVarContext
//           )
//           .map(_.asInstanceOf[Expr[Evolution[Point]]])

//       (Config(serialisedExpr, eitherExprOrError.toOption), State(eitherExprOrError.swap.toOption))
//     }
//   }

//   case class State(message: Option[String])

//   class Backend(bs: BackendScope[StateSnapshot[Config], State]) {
//     def render(snapshot: StateSnapshot[Config], state: State, @silent children: PropsChildren): VdomElement = {
//       val stringSnapshot = StateSnapshot[String](snapshot.value.serialisedExpr) {
//         case (Some(serialized), _) =>
//           if (serialized == snapshot.value.serialisedExpr) Callback.empty
//           else {
//             val (config, state) = Config.from(serialized)
//             snapshot.setState(config) >> bs.setState(state)
//           }
//         case (None, cb) => cb
//       }

//       val component: ConfigComponent[String] = instances.textConfig

//       <.div(
//         ^.className := "dsl-config",
//         component.apply(stringSnapshot)(),
//         <.div(
//           ^.classSet(
//             "dsl-feedback" -> true,
//             "dsl-error" -> state.message.isDefined
//           ),
//           <.span(state.message.getOrElse("").toString)
//         )
//       )

//     }
//   }

//   override val configComponent: ConfigComponent[Config] =
//     japgolly.scalajs.react.ScalaComponent
//       .builder[StateSnapshot[Config]]("DSL Config")
//       // A bit hacky here, we re-parse the expression to load the error message. See TODO above
//       .initialStateFromProps(props => Config.from(props.value.serialisedExpr)._2)
//       .renderBackendWithChildren[Backend]
//       .build

//   override def materialize(ctx: DrawingContext, state: DrawingState[Config]): Iterator[Point] = state.config.expr.map {
//     expr =>
//       data.EvaluationModule.materializeExpr(state.seed, bindPredefinedVars(ctx, expr))
//   }.getOrElse(Iterator.empty)

//   private def varBindings(ctx: DrawingContext): List[(String, AST)] = List(
//     "top" -> AST.DoubleLiteral(ctx.top),
//     "bottom" -> AST.DoubleLiteral(ctx.bottom),
//     "left" -> AST.DoubleLiteral(ctx.left),
//     "right" -> AST.DoubleLiteral(ctx.right)
//   )

//   val initialConfig: Config = Config.from("integrate(point(0, 0), @point(uniform(-2, 2), uniform(-2, 2)))")._1

//   override def configCodec: JsonCodec[Config] =
//     DrawingJsonCodec
// }
// >>>>>>> Stashed changes
