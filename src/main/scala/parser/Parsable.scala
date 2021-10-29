package parser

import parser.models.TreeNode

trait Parsable {

  def parse: Either[String, TreeNode]
}
