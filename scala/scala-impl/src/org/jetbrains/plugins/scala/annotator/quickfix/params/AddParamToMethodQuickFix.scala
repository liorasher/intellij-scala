package org.jetbrains.plugins.scala.annotator.quickfix.params

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.undo.UndoUtil
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile, PsiMethod, ResolveResult, _}
import com.intellij.usageView.UsageInfo
import org.jetbrains.plugins.scala.extensions.{ResolvesTo, _}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScPackage
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScLiteral, ScMethodLike, ScReferenceElement}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScMethodCall, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.ScImportSelectors
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScObject}
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes
import org.jetbrains.plugins.scala.lang.refactoring.changeSignature.changeInfo.ScalaChangeInfo
import org.jetbrains.plugins.scala.lang.refactoring.changeSignature.{ScalaChangeSignatureProcessor, ScalaParameterInfo}
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult
import org.jetbrains.plugins.scala.lang.resolve.processor.DynamicResolveProcessor
import org.jetbrains.plugins.scala.project.ProjectContext

import scala.collection.Seq

class AddParamToMethodQuickFix(reference: ScReferenceElement, methodCall: ScMethodCall) extends IntentionAction {

  private val maybeTargetMethod = findRelatedMethodDefinition(reference)

  override def isAvailable(project: Project, editor: Editor, psiFile: PsiFile): Boolean = {
    val result: Boolean = maybeTargetMethod match {
      case Some(targeMethod) =>
        if (!targeMethod.isValid || targeMethod.getContainingClass == null) false
        else if (maybeTargetMethod.map(createNewParameters).isEmpty) false
        else
          true
      case _ => false
    }
    result
  }

  override def getText: String = {
    val methodName = maybeTargetMethod.map(_.getName).getOrElse("Method")
    s"Add new param to '$methodName'"
  }

  override def invoke(projectT: Project, editor: Editor, psiFile: PsiFile): Unit = {
    val newMethod: ScMethodLike = maybeTargetMethod match {
      case Some(result) => result
      case None => return
    }

    implicit val project: ProjectContext = reference.projectContext

    val processor = new ScalaChangeSignatureProcessor(projectT, ScalaChangeInfo(
      newVisibility = newMethod.getModifierList.accessModifier.fold("")(_.getText),
      function = newMethod,
      newName = newMethod.getName,
      newType = newMethod.getReturnType.toScType(),
      newParams = createNewParameters(newMethod),
      isAddDefaultArgs = false
    )) {
      override def findUsages(): Array[UsageInfo] = {
        UsageInfo.EMPTY_ARRAY
      }
    }
    processor.run()

    println("")
    ApplicationManager.getApplication.runWriteAction(() => UndoUtil.markPsiFileForUndo(psiFile))
  }

  private def findRelatedMethodDefinition(reference: ScReferenceElement): Option[ScMethodLike] = {
    val targets: Set[PsiElement] = reference match {
      case DynamicResolveProcessor.DynamicReference(results) =>
        results.toSet[ResolveResult]
          .map(_.getElement)
          .filterNot(_ == null)
      case referenceElement: ScReferenceElement =>
        referenceElement.multiResolveScala(incomplete = false)
          .toSet[ScalaResolveResult]
          .flatMap {
            case ScalaResolveResult(pkg: ScPackage, _) => packageCase(pkg, Some(reference))
            case result => regularCase(result)
          }
      case ResolvesTo(resolved) =>
        Set(resolved)
      case _ => return None
    }

    val newMethod: ScMethodLike = targets.toSeq.head.asInstanceOf[ScMethodLike]
    Some(newMethod)
  }

  private def createNewParameters(newMethod: ScMethodLike): Seq[Seq[ScalaParameterInfo]] = {
    // TODO: find a way to add not only as last param
    Seq(existingParams(newMethod) :+ newParam(uniqueName(existingParams(newMethod).map(_.name).toSet)))
  }

  @scala.annotation.tailrec
  private def uniqueName(existingNames: Set[String], suffix: Int = 0): String = {
    val suggested = "param" + (if (suffix == 0) "" else String.valueOf(suffix))
    if (!existingNames.contains(suggested)) suggested else uniqueName(existingNames, suffix + 1)
  }

  private def existingParams(newMethod: ScMethodLike): Seq[ScalaParameterInfo] = {
    newMethod.parameterList.clauses.map(_.parameters.map(new ScalaParameterInfo(_))).head
  }

  private def newParam(newParamName: String) = {
    new ScalaParameterInfo(
      name = newParamName,
      oldIndex = -1,
      scType = argumentTypeScType(methodCall.args.getChildren.last),
      project = reference.projectContext,
      isRepeatedParameter = false,
      isByName = false)
  }

  private def argumentTypeScType(element: PsiElement): ScType = {
    implicit val project: ProjectContext = reference.projectContext
    element match {
      case literal: ScLiteral => literal.`type`().right.getOrElse(StdTypes.instance.Any)
      case _: ScMethodCall => StdTypes.instance.Any //TODO find method call return type
      case reference: ScReferenceExpression => reference.`type`().right.getOrElse(StdTypes.instance.Any)
      case _ => StdTypes.instance.Any
    }
  }

  override def getFamilyName: String = {
    getText
  }

  override def startInWriteAction: Boolean = false

  private def regularCase(result: ScalaResolveResult): Seq[PsiElement] = {
    val actualElement = result.getActualElement
    result.element match {
      case function: ScFunction if function.isSynthetic =>
        Seq(function.syntheticCaseClass.getOrElse(actualElement))
      case method: PsiMethod if method.isConstructor && method.getContainingClass == actualElement => Seq(method)
      case element => Seq(actualElement, element) ++ result.innerResolveResult.map(_.getElement)
    }
  }

  private def packageCase(pkg: ScPackage, maybeParent: Option[PsiElement]): Iterable[PsiElement] = {
    import ScalaTokenTypes.{tDOT, tUNDER}
    val maybePackageObject = pkg.findPackageObject(pkg.getResolveScope)

    val maybeSegment = for {
      _ <- maybePackageObject
      parent <- maybeParent
      dot <- Option(parent.getNextSiblingNotWhitespaceComment)
      if dot.getNode.getElementType == tDOT
      segment <- Option(dot.getNextSiblingNotWhitespaceComment)
    } yield segment

    val references = maybeSegment.toSet[PsiElement].flatMap {
      case selectors: ScImportSelectors if !selectors.hasWildcard => selectors.selectors.flatMap(_.reference)
      case _: ScImportSelectors => Seq.empty
      case underscore if underscore.getNode.getElementType == tUNDER => Seq.empty
      case ident => ident.parentOfType(classOf[ScReferenceElement]).toSeq
    }

    val set = references.flatMap(isReferencedFrom)
    val packageRequired = isRequired(set)(_.fromPackage)
    val packageObjectRequired = isRequired(set)(_.fromPackageObject)

    (if (packageRequired) Some(pkg) else None) ++
      (if (packageObjectRequired) maybePackageObject else None)
  }

  private[this] def isReferencedFrom(reference: ScReferenceElement): Option[IsReferenced] =
    reference.multiResolveScala(false) match {
      case Array() => None
      case results => Some(new IsReferenced(results.map(_.element)))
    }

  private[this] def isRequired(set: Set[IsReferenced])
                              (predicate: IsReferenced => Boolean) =
    set.isEmpty || set.exists(predicate)

  private class IsReferenced(elements: Seq[PsiNamedElement]) {

    val (fromPackageObject: Boolean, fromPackage: Boolean) = {
      val (left, right) = elements.partition(isInPackageObject)
      (left.nonEmpty, right.nonEmpty)
    }

    private def isInPackageObject(element: PsiElement): Boolean = element match {
      case member: ScMember if member.isSynthetic => member.getSyntheticNavigationElement.exists(isInPackageObject)
      case _ => element.parentOfType(classOf[ScObject]).exists(_.isPackageObject)
    }
  }

}
