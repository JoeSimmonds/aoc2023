package day8

import scala.util.parsing.combinator._
import scala.collection.mutable
import scala.annotation.tailrec
import cats.instances.boolean

case class Plan(steps: List[Char], network: Map[String, (String, String)]) {
    private lazy val liftedSteps = steps.lift

    private def nextStepIndex(currentStepIndex: Int) = 
        if (currentStepIndex == steps.size-1) {
            0
        } else {
            currentStepIndex + 1
        }

    private val memo: mutable.Map[String, (String, Int)] = mutable.Map.empty

    case class SearchState(stepsTaken: Long, currentNode: String, currentStepIndex: Int) {
        def + (that: SearchState): SearchState = SearchState(this.stepsTaken + that.stepsTaken, that.currentNode, that.currentStepIndex)
    }
    
    @tailrec
    final def stepsUntilNextNodeEndingWithZ(state: SearchState, stepsTaken: Long): SearchState = {
        print(".")
        if(state.currentNode.charAt(2) == 'Z' && stepsTaken > 0)
        {
            println("!")
            state
        } else {
            val nextNode = for {
                s <- liftedSteps(state.currentStepIndex)
                n <- network.get(state.currentNode)
            } yield if (s == 'L') {n._1} else {n._2}

            stepsUntilNextNodeEndingWithZ(SearchState(stepsTaken + 1, nextNode.get, nextStepIndex(state.currentStepIndex)), stepsTaken + 1)
        }
    }

    def stepsWithCache(state: SearchState): SearchState = {
        val memoizedValue = memo.get(s"${state.currentNode}:${state.currentStepIndex}")

        if (memoizedValue.isDefined) {
            SearchState(memoizedValue.get._2, memoizedValue.get._1, (state.currentStepIndex + memoizedValue.get._2) % steps.size)
        } else {
            val result = stepsUntilNextNodeEndingWithZ(state, 0)
            memo.addOne(s"${state.currentNode}:${state.currentStepIndex}" -> (result.currentNode, result.stepsTaken.toInt))
            result
        }
    }

    private def advance1(currentStates: List[SearchState], report: Boolean): List[SearchState] = {
        val sorted = currentStates.sortBy(_.stepsTaken)
        val t = sorted.head
        val l = sorted.last
        if (report) {
            println(s"currentStates = $currentStates - Range = ${l.stepsTaken - t.stepsTaken} - Advancing $t")
        }
        val next = stepsWithCache(t)
        (t + next) :: sorted.tail
    }

    @tailrec
    private def doCountForGhosts(currentStates: List[SearchState], recs: Long): Long = {
        if (currentStates.forall(_.currentNode.charAt(2) == 'Z') && currentStates.groupBy(_.stepsTaken).size == 1) {
            currentStates.head.stepsTaken
        } else {
            doCountForGhosts(advance1(currentStates, recs % 250000 == 0), recs +1)
        }
    }

    def doCountForGhosts(): Long = {
        val currentNodes = network.keys.filter(_.charAt(2) == 'A')
        doCountForGhosts(currentNodes.map(n => SearchState(0, n, 0)).toList, 0)
    }
}


class NetworkParser extends RegexParsers {
    private val networkInProgress: mutable.Map[String, (String, String)] = mutable.Map.empty

    val step: Parser[Char] = 'R' | 'L' ^^ {identity}
    val steps: Parser[List[Char]] = step.*
    val nodeName: Parser[String] = """[A-Z,\d][A-Z,\d][A-Z,\d]""".r
    val node: Parser[(String, (String, String))] = nodeName ~ ("=" ~> "(" ~> nodeName <~ ",") ~ (nodeName <~ ")") ^^ {case n ~ l ~ r => (n, (l, r))}
    val network: Parser[Map[String, (String, String)]] = (node ^^ (n => networkInProgress.addOne(n))).* ^^ {_ => networkInProgress.toMap}
    val plan: Parser[Plan] = steps ~ network ^^ {case s ~ n => Plan(s, n)}
}

@tailrec
def doCount(m: Map[String, (String, String)], steps: List[Char], currentStepIndex: Int, currentCount: Long, currentNode: String): Long = {
    if (currentNode == "ZZZ") {
        currentCount
    } else {
        val nextStepIndex = if (currentStepIndex == steps.size-1) {
            0
        } else {
            currentStepIndex + 1
        }

        val nextNode:String = if(steps(currentStepIndex) == 'L') {
            m.get(currentNode).map(_._1).get
        } else {
            m.get(currentNode).map(_._2).get
        }

        doCount(m, steps, nextStepIndex, currentCount+1, nextNode)
    }
}

def countSteps(input: String): Long = {
    val p = new NetworkParser()
    val plan = p.parse(p.plan, input).get 
    doCount(plan.network, plan.steps, 0, 0, "AAA" )
}

def countStepsForGhosts(input: String): Long = {
    val p = new NetworkParser()
    val plan = p.parse(p.plan, input).get
    plan.doCountForGhosts()
}
