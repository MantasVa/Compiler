package helpers

import parser.models.TreeNode

object Printer {

  def printBinary(tree: TreeNode): Unit = {
    def go(branch: TreeNode): Unit = {

      println(s"${branch.token.tokenType} ${branch.token.value}")

      if (!branch.child.isEmpty) {
        branch.child.foreach(t => go(t))
      }

      if (!branch.sibling.isEmpty) {
        branch.sibling.foreach(t => go(t))
      }
    }

    go(tree)
  }
}
