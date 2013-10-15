package de.up.ling.tulipac

import scala.util.parsing.combinator._
import java.io.Reader

class TagParser extends RegexParsers {
	def parseGrammar(reader: Reader) : Seq[TopLevel] = {
	  parseAll(phrase(grammar), reader) match {
	    case Success(g, _) => g
	    case x: Failure => throw new RuntimeException(x.toString())
	    case x: Error => throw new RuntimeException(x.toString())
	  }
	}
  
  
    // ignore ordinary whitespace, line comments, and inline comments between combinators
    // adapted from http://pastebin.com/2GvtvSPb
    override val whiteSpace = "(?sm)(\\s*(?://.*?$|/\\*((?!\\*/).)*\\*/)\\s*|\\s+)+".r
  
    // \p{L} should match any (Unicode) letter, but somehow it still doesn't work
	def rawIdentifier = """[\p{L}_][\p{L}_0-9]*""".r ^^ { s => new Identifier(s) }
	def quotedIdentifier = """\'[^']*\'""".r ^^ { s => new Identifier(s.substring(1, s.length()-1))}
	def doubleQuotedIdentifier = """\"[^"]*\"""".r ^^ { s => new Identifier(s.substring(1, s.length()-1))}
	def familyIdentifier = """<[^>]*>""".r ^^ { s => new FamilyIdentifier(s.substring(1, s.length()-1))}
	def annotation = """@[\p{L}_0-9]+""".r ^^ { s => new Annotation(s.substring(1)) }
	def variable = """\?[\p{L}_0-9]+""".r ^^ { s => new Variable(s.substring(1)) }
	
	def substitutionMarker = """!""".r ^^ { _ => new SubstitutionMarker() }
	def footMarker = """\*""".r ^^ { _ => new FootMarker() }
	def anchorMarker = """\+""".r ^^ { _ => new AnchorMarker() }
	
	def identifier : Parser[Identifier] = rawIdentifier | quotedIdentifier | doubleQuotedIdentifier ^^ { m => m }
	def marker : Parser[Marker] = substitutionMarker | footMarker | anchorMarker ^^ { m => m }

	def grammar : Parser[Seq[TopLevel]] = rep(tree | family | wordByItself | lemma | include) ^^ { g => g }
	
	/**** trees ****/
	def tree : Parser[Tree] = "tree" ~ identifier ~ ":" ~ node ^^ {
	  case _ ~ name ~ _ ~ tree => new Tree(name.str, tree)
	}
	
	private def get(fs:Option[Map[String,StringValued]]) = fs.getOrElse(Map())
	
	def node : Parser[Node] = identifier ~ rep(annotation | marker) ~ opt(fs) ~ opt(fs) ~ opt("{" ~ rep(node) ~ "}") ^^ {
	  case cat ~ markers ~ topOpt ~ botOpt ~ childrenOpt =>
	    val top = get(topOpt)
	    val bot = get(botOpt)	    
	    val children : Seq[Node] = childrenOpt match {
	      case Some(_ ~ co ~ _) => co
	      case None => Nil
	    }
	    
	    new Node(cat.str, markers, top, bot, children)
	}
	
	def ft : Parser[(String,StringValued)] = identifier ~ "=" ~ (identifier | variable) ^^ {
	  case key ~ _ ~ (value:StringValued) => (key.str -> value)
	}
	
	def fs : Parser[Map[String,StringValued]] = "[" ~ repsep(ft,",") ~ "]" ^^ {
	  case _ ~ fts ~ _ => Map() ++ fts
	}
	
	/**** tree families ****/
	def family : Parser[TreeFamily] = "family" ~ identifier ~ ":" ~ "{" ~ repsep(identifier, ",") ~ "}" ^^ {
	  case _ ~ name ~ _ ~ _ ~ trees ~ _ => new TreeFamily(name.str, trees.map { t => t.str })
	}
	
	
	/**** words ****/
	def wordByItself : Parser[Words] = "word" ~ identifier ~ ":" ~ (identifier|familyIdentifier) ~ opt(fs) ^^ {
	  case _ ~ word ~ _ ~ treeName ~ fs => new Words(Array(new Word(word.str, word.str, treeName, get(fs))))
	}
	
	def wordInLemma : Parser[Word] = "word" ~ identifier ~ opt(":" ~ fs) ^^ {
	  case _ ~ word ~ rest => rest match {
	    case Some(_ ~ fs) => new Word(word.str, null, null, fs)
	    case None         => new Word(word.str, null, null, Map())     
	  } 
	}
	
	def lemma : Parser[Words] = "lemma" ~ identifier ~ ":" ~ (identifier|familyIdentifier) ~ opt(fs) ~ "{" ~ rep(wordInLemma) ~ "}" ^^ {
	  case _ ~ lemma ~ _ ~ treeName ~ fs ~ _ ~ words ~ _ => new Words(words.map { word:Word => new Word(word.word, lemma.str, treeName, get(fs) ++ word.fs) })
	}
	
	/**** #include ****/
	def include : Parser[Include] = "#include" ~ identifier ^^ {
	  case _ ~ filename => new Include(filename.str)
	}
}


sealed abstract class Expression

class StringValued(val str:String) extends Expression 
case class Identifier(string:String) extends StringValued(string)
case class FamilyIdentifier(string:String) extends StringValued(string)
case class Annotation(string:String) extends StringValued(string)
case class Variable(string:String) extends StringValued(string) {
  def strWithAt = "@" + string 
}

class Marker extends Expression
case class SubstitutionMarker extends Marker
case class FootMarker extends Marker
case class AnchorMarker extends Marker

case class Node(cat:String, markers:Seq[Expression], top:Map[String,StringValued], bottom:Map[String,StringValued], children:Seq[Node]) extends Expression {
  def nodeType = {
    if( markers.contains(new SubstitutionMarker()) ) {
      "subst"
    } else if( markers.contains(new AnchorMarker())) {
      "anchor"
    } else if( markers.contains(new FootMarker())) {
      "foot"
    } else if( markers.contains(new Annotation("NA"))) {
      "nadj"
    } else {
      "std"
    }
  }  
}

class TopLevel extends Expression
case class Tree(name:String, tree:Node) extends TopLevel
case class TreeFamily(name:String, trees:Seq[String]) extends TopLevel
case class Word(word:String, lemma:String, trees:StringValued, fs:Map[String,StringValued])
case class Words(words:Seq[Word]) extends TopLevel
case class Include(filename:String) extends TopLevel

