### OCR: Tesseract
### Fecha de elaboración: 14 de octubre de 2021
### Fecha de actualización: 14 de octubre de 2021


# 0. Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tesseract, pdftools)

tesseract_download("spa")

esp <- tesseract("spa")

text <- ocr("https://raw.githubusercontent.com/jmtoral/act_intro/main/01_datos/Captura.PNG",
            engine = esp)

cat(text)


# PDF sin texto -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pngfile <- pdftools::pdf_convert("01_datos/41intro (1).pdf", pages = c(2,3), dpi = 300)

text <- ocr("01_datos/Captura2.PNG",
            engine = esp)

cat(text)
