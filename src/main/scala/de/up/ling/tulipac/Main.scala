package de.up.ling.tulipac

import scala.xml.Elem
import java.io._
import com.google.common.io.Files

object Main {
	private val treeToFamily = scala.collection.mutable.Map[String,String]()
	private val familyToTree = scala.collection.mutable.Map[String, TreeFamily]()
	private val treeNameToTree = scala.collection.mutable.Map[String, Tree]()
	private val treeList = scala.collection.mutable.ListBuffer[Tree]()
	private val wordList = scala.collection.mutable.ListBuffer[Word]()
	
	def main(args: Array[String]) : Unit = {
	  val parser = new TagParser()	    
	  val inputFile = new File(args(0))
	  val filenameStem = Files.getNameWithoutExtension(args(0))
	  
	  processGrammar(inputFile, parser, "")
	  println("Done parsing.\n")
	  
	  val gFilename = new File(filenameStem + "-g.xml")
	  println(s"Writing grammar file ${gFilename} ...")
	  val gFile = new PrintWriter(gFilename)
	  xml.XML.write(gFile, mkGrammar(treeList), "UTF-8", xmlDecl = true, doctype = xml.dtd.DocType("mcgrammar", xml.dtd.SystemID("xmg-mctag.dtd.xml"), Nil))
	  gFile.close()
	  
	  val mFilename = new File(filenameStem + "-m.xml")
	  println(s"Writing morphology file ${mFilename} ...")
	  val mFile = new PrintWriter(mFilename)
	  xml.XML.write(mFile, mkMorph(wordList), "UTF-8", xmlDecl = true, doctype = xml.dtd.DocType("mcgrammar", xml.dtd.SystemID("xmg-mctag.dtd.xml"), Nil))
	  mFile.close()
	  
	  val lFilename = new File(filenameStem + "-l.xml")
	  println(s"Writing lexicon file ${lFilename} ...")
	  val lFile = new PrintWriter(lFilename)
	  xml.XML.write(lFile, mkLex(wordList), "UTF-8", xmlDecl = true, doctype = xml.dtd.DocType("mcgrammar", xml.dtd.SystemID("xmg-mctag.dtd.xml"), Nil))
	  lFile.close()
	  
	  println("Done.")
	}
	
	
	def processGrammar(file:File, parser:TagParser, printPrefix:String) : Unit = {
	  val reader = new FileReader(file)
	  
	  println(printPrefix + s"Parsing ${file} ...")
	  val grammar: Seq[TopLevel] = try {
	    parser.parseGrammar(reader)
	  } catch {
	    case e: Exception => println(e.getMessage()); System.exit(1); null
	  }  
	  
	  for( topLevel <- grammar ) {
	    topLevel match {
	      case tf : TreeFamily => for( treeName <- tf.trees ) { treeToFamily(treeName) = tf.name; familyToTree(tf.name) = tf } 
	      case tree : Tree => treeList += tree; treeNameToTree(tree.name) = tree
	      case words : Words => wordList ++= words.words
	      case Include(includedFile) => processGrammar(new File(includedFile), parser, "  " + printPrefix)
	    }
	  }
	}
	
	
	
	
	
	def mkGrammar(inputs:Seq[Tree]) = {
	  <mcgrammar>
		{for (input <- inputs) yield { toXml(input) }}
	  </mcgrammar>
	}
	
	def mkMorph(words:Seq[Word]) = {
	  <mcgrammar>
        <morphs>
		  { for(word <- words) yield {
		      val cat = anchorCat(word.trees).getOrElse("***")
		      
		      if( cat == "***" ) {
		        println("Warning: cannot determine anchor for word " + word.word)
		      }
		      
		      <morph lex={word.word}>
		        <lemmaref cat={cat} name={word.lemma}>
		          {toXml(word.fs)}
		        </lemmaref>
		      </morph>
		    }
		  }
		</morphs>
	  </mcgrammar>
	}
	
	// TODO:
	// - group duplicate lemma entries
	
	def mkLex(words:Seq[Word]) = {
      <mcgrammar>
        <lemmas>
		  { for( word <- words ) yield {
		      val cat = anchorCat(word.trees).getOrElse("***")
		      
		      if( cat == "***" ) {
		        println("Warning: cannot determine anchor for word " + word.word)
		      }
		      
		      <lemma name={word.lemma} cat={cat}>
		        <anchor tree_id={formatTreeName(word.trees)} />
		      </lemma>
		    }
		  }
		</lemmas>
	  </mcgrammar>
	}
	
	def formatTreeName(treeId:StringValued) = treeId match {
	  case tree : Identifier => tree.str
	  case family : FamilyIdentifier => s"family[@name=${family.str}]"
	}
	
	def toXml(tree:Tree) : Elem = {
	  <entry type="anc" name={tree.name}>
	    <family>{getFamily(tree) match { case Some(tf) => tf   case None => tree.name }}</family>
	  	<trace />
	  	
	  	<tree id={tree.name}>
	  	  {toXml(tree.tree)}
	  	</tree>

	  	<semantics></semantics>
	  	<interface></interface>
	  </entry>
	}
	
	
  def toXml(label:String, fs:Map[String,StringValued]) : Elem = {
    <f name={label}>
      {toXml(fs)}
    </f>
  }
  
  def toXml(fs:Map[String,StringValued]) : Elem = {
    <fs>
    	{for( key <- fs.keys ) yield {
    	    fs(key) match {
    	      case value:Identifier => <f name={key}> <sym value={value.str} /> </f>
    	      case value:Variable =>   <f name={key}> <sym varname={value.strWithAt} /> </f>
    	    }
    	  }
    	}
      </fs>
  }
  
  def toXml(node:Node) : Elem = {
    <node type={node.nodeType}>
      <narg>
    	<fs>
    		<f name="cat"> <sym value={node.cat} /> </f>
    		{toXml("top", node.top)}
    		{toXml("bot", node.bottom)}
    	</fs>
      </narg>

      {for( child <- node.children ) yield toXml(child)}
    </node>
  }
  
  
  
  
  private def anchorCat(tree:Node) : Option[String] = {
    if( tree.nodeType == "anchor" ) {
      Some(tree.cat)
    } else {
      val ret = tree.children.map { anchorCat(_) }.flatten
      
      if( ret.isEmpty ) {
        None
      } else {
        Some(ret(0))
      }
    }
  }
  
  private def anchorCat(family:TreeFamily) : Option[String] = treeNameToTree.get (family.trees(0)).flatMap { tree:Tree => anchorCat(tree.tree) }
  
  private def anchorCat(name: StringValued) : Option[String] = {
    name match {
      case Identifier(treename) => treeNameToTree.get(treename).flatMap { tree:Tree => anchorCat(tree.tree) }
      case FamilyIdentifier(familyname) => familyToTree.get(familyname).flatMap { family : TreeFamily => anchorCat(family) }
    }
  }
  
  private def getFamily(tree:Tree) = treeToFamily.get(tree.name)
}

