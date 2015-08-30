/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package org.scalaide.ui.internal.repl

import org.scalaide.core.IScalaPlugin
import org.scalaide.ui.syntax.ScalariformToSyntaxClass
import org.eclipse.jdt.internal.ui.JavaPlugin
import org.eclipse.jdt.ui.PreferenceConstants
import org.eclipse.jface.resource.JFaceResources
import org.eclipse.swt.SWT
import org.eclipse.swt.custom.PaintObjectEvent
import org.eclipse.swt.custom.PaintObjectListener
import org.eclipse.swt.custom.SashForm
import org.eclipse.swt.custom.StyleRange
import org.eclipse.swt.events.DisposeEvent
import org.eclipse.swt.events.DisposeListener
import org.eclipse.swt.events.VerifyEvent
import org.eclipse.swt.events.VerifyListener
import org.eclipse.swt.graphics.Color
import org.eclipse.swt.graphics.Font
import org.eclipse.swt.graphics.GlyphMetrics
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.graphics.ImageData
import org.eclipse.swt.graphics.PaletteData
import org.eclipse.swt.graphics.RGB
import org.eclipse.swt.graphics.Rectangle
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.Caret
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.part.ViewPart
import scalariform.lexer.ScalaLexer
import java.awt._
import java.awt.image._
import aalto.smcl.interfaces.GlobalMetadataInterfaceSourceProviderRegistry
import aalto.smcl.interfaces.MetadataInterfaceSourceProvider
import aalto.smcl.interfaces.StaticGeneralBitmapSource
import aalto.smcl.interfaces.StaticThumbnailBitmapSource
import aalto.smcl.interfaces.ResourceMetadataSource
import aalto.smcl.interfaces.ProviderMetadataSource




object InterpreterConsoleView {
  val BackgroundColor = "org.scalaide.ui.color.interpreterBackground"
  val ForegroundColor = "org.scalaide.ui.color.interpreterForeground"
  val ErrorForegroundColor = "org.scalaide.ui.color.interpreterErrorForeground"
  val LineNumberBackgroundColor = "org.scalaide.ui.color.lineNumberBackground"
}

/**
 * A split horizontal view for enter scala commands and displaying REPL output.
 *
 * This UI component contains a sash form with the top widget being a console-output like text view
 * and the bottom view being an instance of `CommandField` for entering scala expressions.
 */
trait InterpreterConsoleView extends ViewPart {
  import InterpreterConsoleView._

  val ImagePaddingTopInPixels = 10
  val ImagePaddingBottomInPixels = 20

  protected var interpreterPanel: SashForm = null
  protected var resultsTextWidget: StyledTextWithSimpleMenu = null
  protected var inputCommandField: CommandFieldWithLineNumbersAndMenu = null
  protected var codeBgColor: Color = null
  protected var codeFgColor: Color = null
  protected var errorFgColor: Color = null
  protected var display: Display = null
  protected def doOnLineNumbersVisibilityUpdate(enabled: Boolean): Unit = {}

  protected def createCommandField(parent: Composite, suggestedStyles: Seq[Int]): CommandFieldWithLineNumbersAndMenu = {
    new CommandFieldWithLineNumbersAndMenu(parent, suggestedStyles.reduce((l, r) => l | r)) {
      override protected def helpText = "<type an expression>\tCTRL+ENTER to evaluate\nto browse expressions from history use:\tCTRL+Up and CTRL+Down"
      setEvaluator(new CommandField.Evaluator {
        override def eval(command: String) = evaluate(command)
      })

      override def onLineNumbersVisibilityUpdated(enabled: Boolean): Unit = doOnLineNumbersVisibilityUpdate(enabled)
    }
  }

  /** Override to perform some specific work (such as performing evaluation and updating the top output) on scala command evaluation */
  protected def evaluate(command: String): Unit

  /**
   * Creates view with uneditable text field for result and editable text field for input
   */
  protected def createInterpreterPartControl(parent: Composite): Unit = {
    display = parent.getDisplay()

    val reg = JFaceResources.getColorRegistry
    codeBgColor = reg.get(BackgroundColor)
    codeFgColor = reg.get(ForegroundColor)
    errorFgColor = reg.get(ErrorForegroundColor)

    interpreterPanel = new SashForm(parent, SWT.VERTICAL)
    interpreterPanel.setLayout(new FillLayout)

    // 1st row
    resultsTextWidget = new StyledTextWithSimpleMenu(interpreterPanel, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL)
    resultsTextWidget.setLayout(new FillLayout)
    resultsTextWidget.setEditable(false)
    resultsTextWidget.setCaret(new Caret(resultsTextWidget, SWT.NONE))
    resultsTextWidget.setAlwaysShowScrollBars(false)
    resultsTextWidget.setBackground(reg.get(BackgroundColor))

    val editorFont = JFaceResources.getFont(PreferenceConstants.EDITOR_TEXT_FONT)
    resultsTextWidget.setFont(editorFont) // java editor font

    resultsTextWidget.addVerifyListener(new VerifyListener()  {
      override def verifyText(event: VerifyEvent): Unit = {
        if (event.start == event.end)
          return

        val text = resultsTextWidget.getText(event.start, event.end - 1)

        var index = text.indexOf('\uFFFC')
        while (index != -1) {
          val style = resultsTextWidget.getStyleRangeAtOffset(event.start + index)
          if (style != null) {
            val image: Image = style.data.asInstanceOf[Image]

          if (image != null)
              image.dispose()
          }

          index = text.indexOf('\uFFFC', index + 1)
        }
      }
    });

    resultsTextWidget.addPaintObjectListener(new PaintObjectListener() {
      override def paintObject(event: PaintObjectEvent): Unit = {
        val style: StyleRange = event.style
        if (style != null) {
          val image: Image = style.data.asInstanceOf[Image]

          if (!image.isDisposed()) {
            val x = event.x
            val y = event.y + event.ascent - style.metrics.ascent + ImagePaddingTopInPixels
            event.gc.drawImage(image, x, y)
          }
        }
      }
    });

    resultsTextWidget.addDisposeListener(new DisposeListener() {
      override def widgetDisposed(event: DisposeEvent): Unit = {
        val styles: Array[StyleRange] = resultsTextWidget.getStyleRanges()

        for (i <- 0 to (styles.length - 1)) {
          val style: StyleRange = styles(i)

          if (style.data != null) {
            val image: Image = style.data.asInstanceOf[Image]

            if (image != null)
              image.dispose()
          }
        }
      }
    });

    // 2nd row
    inputCommandField = createCommandField(interpreterPanel, Seq(SWT.BORDER, SWT.MULTI, SWT.H_SCROLL, SWT.V_SCROLL, SWT.RESIZE))
    inputCommandField.setFont(editorFont)
    inputCommandField.setLayout(new FillLayout)
    inputCommandField.setAlwaysShowScrollBars(false)

    interpreterPanel.setWeights(Array(3, 1))
  }

  /**
   * Display the string with code formatting
   */
  protected def displayCode(text: String) = displayPadded(codeBgColor) {
    val prefStore = IScalaPlugin().getPreferenceStore
    for (token <- ScalaLexer.rawTokenise(text, forgiveErrors = true)) {
      val textAttribute = ScalariformToSyntaxClass(token).getTextAttribute(prefStore)
      val bgColor = Option(textAttribute.getBackground) getOrElse codeBgColor
      appendText(token.text, textAttribute.getForeground, bgColor, textAttribute.getStyle, insertNewline = false)
    }
    appendText("\n", codeFgColor, codeBgColor, SWT.NORMAL, insertNewline = false)
  }

  protected def displayObject(resultObjectOption: Option[Any]): Unit = {
    if (resultObjectOption.isEmpty)
      return

    val resultObject = resultObjectOption.get

    val providerSetOption = GlobalMetadataInterfaceSourceProviderRegistry.queryProvidersFor(resultObject)
    if (providerSetOption == None)
      return

    var metaInterfaceSourceOption: Option[Any] = None
    providerSetOption.get.find { provider =>
      metaInterfaceSourceOption = provider.querySourceFor(resultObject)
      metaInterfaceSourceOption.isDefined
    }

    if (metaInterfaceSourceOption.isEmpty)
      return

    val metaInterfaceSource = metaInterfaceSourceOption.get

    if (metaInterfaceSource.isInstanceOf[StaticThumbnailBitmapSource]) {
      val thumbnailSource = metaInterfaceSource.asInstanceOf[StaticThumbnailBitmapSource]

      if (thumbnailSource.numberOfThumbnailBitmaps() < 1)
        return

      val maximumWidthInPixels: Int = 1000
      val maximumHeightInPixels: Int = 200
      val buffers = thumbnailSource.thumbnailBitmapsOption(maximumWidthInPixels, maximumHeightInPixels)

      buffers.get.foreach { appendBitmap(_) }
    }
    else if (metaInterfaceSource.isInstanceOf[StaticGeneralBitmapSource]) {
      val generalBitmapSource = metaInterfaceSource.asInstanceOf[StaticGeneralBitmapSource]

      if (generalBitmapSource.numberOfGeneralBitmaps() < 1)
        return

      val buffers = generalBitmapSource.generalBitmapsOption()
      buffers.get.foreach { appendBitmap(_) }
    }

    appendText("\n", codeFgColor, codeBgColor, SWT.NORMAL, insertNewline = false)
  }

  protected def appendBitmap(buffer: BufferedImage): Unit = {
    val colorModel: DirectColorModel =
      buffer.getColorModel.asInstanceOf[DirectColorModel]

    val palette: PaletteData =  new PaletteData(
        colorModel.getRedMask(),
        colorModel.getGreenMask(),
        colorModel.getBlueMask())

    val imageData: ImageData = new ImageData(
        buffer.getWidth(),
        buffer.getHeight(),
        colorModel.getPixelSize(),
        palette)

    for (y <- 0 to (imageData.height - 1); x <- 0 to (imageData.width - 1)) {
      val rgb = buffer.getRGB(x, y)

      val pixel: Int = palette.getPixel(new RGB((rgb >> 16) & 0xFF, (rgb >> 8) & 0xFF, rgb & 0xFF))

      imageData.setPixel(x, y, pixel)
      imageData.setAlpha(x, y, (rgb >> 24) & 0xFF)
    }

    val display: Display = Display.getCurrent
    val image: Image = new Image(display, imageData)

    val imageBounds: Rectangle = image.getBounds

    resultsTextWidget.append("\uFFFC")
    val newStyle = new StyleRange()
    newStyle.start = (resultsTextWidget.getCharCount - 1)
    newStyle.length = 1
    newStyle.data = image
    newStyle.metrics =
      new GlyphMetrics(
          imageBounds.height + ImagePaddingTopInPixels + ImagePaddingBottomInPixels,
          0,
          imageBounds.width + 20)

    resultsTextWidget.setStyleRange(newStyle)
  }

  protected def displayOutput(text: String) = displayPadded(codeBgColor) {
    appendText(text + "\n", codeFgColor, codeBgColor, SWT.NORMAL)
  }

  protected def displayError(text: String) = displayPadded(codeBgColor) {
    appendText(text + "\n", errorFgColor, codeBgColor, SWT.NORMAL)
  }

  protected def displayPadded(bgColor: Color)(display: => Unit) = {
    insertSpacing(bgColor)
    display
  }

  private def insertSpacing(bgColor: Color) = {
    val fontData = resultsTextWidget.getFont().getFontData()
    fontData.foreach(_.setHeight(4))
    val font = new Font(display, fontData)
    appendText("\n ", null, bgColor, SWT.NORMAL, font = font)
  }

  protected def appendText(text: String, fgColor: Color, bgColor: Color, fontStyle: Int, font: Font = null, insertNewline: Boolean = false) = {
    val lastOffset = resultsTextWidget.getCharCount
    val oldLastLine = resultsTextWidget.getLineCount

    val outputStr =
      if (insertNewline) "\n" + text.stripLineEnd + "\n\n"
      else text

    resultsTextWidget.append(outputStr)
    val style1 = new StyleRange(lastOffset, outputStr.length, fgColor, null, fontStyle)
    style1.font = font
    resultsTextWidget.setStyleRange(style1)

    val lastLine = resultsTextWidget.getLineCount
    if (bgColor != null)
      resultsTextWidget.setLineBackground(oldLastLine - 1, lastLine - oldLastLine, bgColor)
    resultsTextWidget.setTopIndex(resultsTextWidget.getLineCount - 1)
    val style2 = new StyleRange(lastOffset, outputStr.length, fgColor, null, fontStyle)
    style2.font = font
    resultsTextWidget.setStyleRange(style2)
  }

  override def dispose(): Unit = {
    if (interpreterPanel != null) interpreterPanel.dispose()
    if (inputCommandField != null) inputCommandField.dispose()
    if (resultsTextWidget != null) resultsTextWidget.dispose()
  }
}
