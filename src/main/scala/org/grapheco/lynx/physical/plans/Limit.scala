package org.grapheco.lynx.physical.plans

import org.grapheco.lynx.dataframe.DataFrame
import org.grapheco.lynx.evaluator.ExpressionContext
import org.grapheco.lynx.physical.{ExecuteException, PhysicalPlannerContext}
import org.grapheco.lynx.runner.ExecutionContext
import org.grapheco.lynx.types.property.LynxInteger
import org.opencypher.v9_0.expressions.Expression

case class Limit(expr: Expression)(l: PhysicalPlan, val plannerContext: PhysicalPlannerContext)
  extends SinglePhysicalPlan(l) {

  override def execute(implicit ctx: ExecutionContext): DataFrame = {
    val df = in.execute(ctx)
    implicit val ec: ExpressionContext = ctx.expressionContext
    val limit: Long = eval(expr) match {
      case LynxInteger(n) => n
      case _ => throw ExecuteException("The result of LIMIT expression must is a integer.")
    }
    df.take(limit.intValue())
  }

}
