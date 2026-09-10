"""Aggiunge a presentazione_8sett26.pptx una slide "Flusso di lavoro MO.V.O.T".

Flusso orizzontale a 5 passi, colorati per attore; sotto a sinistra la banda
infrastruttura. Metà destra e parte bassa lasciate libere per testi/considerazioni.

Input:  presentazione_8sett26.pptx (stessa cartella dello script)
Output: lo stesso file, con la slide inserita in POSIZIONE (1-based)

Uso (da terminale, nella cartella presentazione/):
    pip install python-pptx      # solo la prima volta
    python aggiungi_slide_flusso_movot.py
Chiudere PowerPoint prima di lanciarlo.
"""
from pathlib import Path
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.dml.color import RGBColor
from pptx.enum.shapes import MSO_SHAPE, MSO_CONNECTOR
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR
from pptx.oxml.ns import qn
from lxml import etree

# Parametri ---
FILE = Path(__file__).parent / "presentazione_8sett26.pptx"
POSIZIONE = 4          # dove inserire la slide (dopo "Integrare i 2 sistemi...")
TITOLO = "Flusso di lavoro MO.V.O.T"

# palette del deck (stessa della slide 3)
COL = {
    "Area Erogativa":   RGBColor(0x4E, 0xB3, 0xA5),
    "Infor":            RGBColor(0xE0, 0x7B, 0x54),
    "Enti beneficiari": RGBColor(0x7F, 0x9D, 0xB9),
    "Osservatorio":     RGBColor(0x0E, 0x66, 0x55),
    "Infrastruttura":   RGBColor(0x88, 0x88, 0x88),
}
GREY_T, GREY_M, WHITE = RGBColor(0x33, 0x33, 0x33), RGBColor(0x66, 0x66, 0x66), RGBColor(0xFF, 0xFF, 0xFF)

# passi del flusso: (numero, etichetta, sottotitolo, attore)
PASSI = [
    ("1", "Lista da invitare",       "Excel: richiedenti / bando / anno", "Area Erogativa"),
    ("2", "Verifiche e attivazione", "controlli di sicurezza",            "Infor"),
    ("3", "Compilazione online",     "link sul sito, password, stato pratica", "Enti beneficiari"),
    ("4", "Salvataggio",             "tabelle SQL Server",                "Infrastruttura"),
]
USCITE = [   # passo 5, due uscite
    ("5a", "Grafici ed Excel",  "aggregati dall'applicazione", "Area Erogativa"),
    ("5b", "Vista sul DB",      "domanda + risposte, via VPN", "Osservatorio"),
]

# Funzioni ---
prs = Presentation(FILE)
slide = prs.slides.add_slide(prs.slide_layouts[6])  # layout "Blank"

def f_testo(sh, righe, align=PP_ALIGN.LEFT, anchor=MSO_ANCHOR.MIDDLE, margine=0.05):
    """righe = lista di (testo, size, colore, bold)"""
    tf = sh.text_frame
    tf.word_wrap, tf.vertical_anchor = True, anchor
    for m in ("margin_left", "margin_right", "margin_top", "margin_bottom"):
        setattr(tf, m, Inches(margine))
    for i, (t, size, colore, bold) in enumerate(righe):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        p.alignment = align
        r = p.add_run(); r.text = t
        r.font.name, r.font.size, r.font.bold, r.font.color.rgb = "Calibri", Pt(size), bold, colore

def f_box(x, y, w, h, righe=(), fill=WHITE, line=None, lw=1, forma=MSO_SHAPE.ROUNDED_RECTANGLE,
          align=PP_ALIGN.LEFT, tratteggio=False):
    sh = slide.shapes.add_shape(forma, Inches(x), Inches(y), Inches(w), Inches(h))
    if forma == MSO_SHAPE.ROUNDED_RECTANGLE: sh.adjustments[0] = 0.15
    sh.shadow.inherit = False
    if fill is None: sh.fill.background()
    else: sh.fill.solid(); sh.fill.fore_color.rgb = fill
    if line is None: sh.line.fill.background()
    else:
        sh.line.color.rgb, sh.line.width = line, Pt(lw)
        if tratteggio: sh.line.dash_style = 4
    if righe: f_testo(sh, righe, align=align)
    return sh

def f_freccia(x1, y1, x2, y2, colore=GREY_M, gomito=False):
    c = slide.shapes.add_connector(MSO_CONNECTOR.ELBOW if gomito else MSO_CONNECTOR.STRAIGHT,
                                   Inches(x1), Inches(y1), Inches(x2), Inches(y2))
    c.line.color.rgb, c.line.width = colore, Pt(1.25)
    coda = etree.SubElement(c.line._get_or_add_ln(), qn("a:tailEnd"))
    coda.set("type", "triangle"); coda.set("w", "med"); coda.set("len", "med")
    return c

def f_passo(x, y, w, h, n, etichetta, sotto, attore):
    """box bianco bordato col colore dell'attore + numero nel cerchietto"""
    c = COL[attore]
    b = f_box(x, y, w, h, [(etichetta, 9.5, c, True), (sotto, 8, GREY_T, False)], line=c, lw=1.5)
    b.text_frame.margin_left = Inches(0.22)
    f_box(x - 0.14, y - 0.14, 0.34, 0.34, [(n, 8, WHITE, True)], fill=c, forma=MSO_SHAPE.OVAL,
          align=PP_ALIGN.CENTER)
    return b

# Titolo e legenda attori ---
t = slide.shapes.add_textbox(Inches(0.2), Inches(0.08), Inches(9.6), Inches(0.45))
f_testo(t, [(TITOLO, 16, GREY_T, True)], margine=0.02)

x = 0.3
for attore, c in COL.items():
    f_box(x, 0.6, 0.18, 0.18, fill=c, forma=MSO_SHAPE.OVAL)
    lb = slide.shapes.add_textbox(Inches(x + 0.2), Inches(0.53), Inches(1.5), Inches(0.3))
    f_testo(lb, [(attore, 8.5, GREY_M, False)], margine=0.02)
    x += 0.25 + 0.075 * len(attore) + 0.35

# Flusso orizzontale ---
Y, H, W, GAP = 1.15, 0.75, 1.55, 0.3
box = []
for i, (n, et, sotto, attore) in enumerate(PASSI):
    box.append(f_passo(0.4 + i * (W + GAP), Y, W, H, n, et, sotto, attore))
for a, b in zip(box, box[1:]):
    f_freccia(a.left / 914400 + W, Y + H / 2, b.left / 914400 + GAP + W, Y + H / 2)

# passo 5: due uscite impilate a destra del passo 4
x5 = box[-1].left / 914400 + W + GAP
h5 = 0.62
for j, (n, et, sotto, attore) in enumerate(USCITE):
    y5 = Y - 0.35 + j * (h5 + 0.2)
    u = f_passo(x5, y5, W, h5, n, et, sotto, attore)
    f_freccia(x5 - GAP, Y + H / 2, x5, y5 + h5 / 2, gomito=True)

# Infrastruttura (in basso a sinistra, sotto il passo 4) ---
XI, YI, WI, HI = 0.4, 2.55, 5.2, 1.05
f_box(XI, YI, WI, HI, line=COL["Infrastruttura"], tratteggio=True)
lb = slide.shapes.add_textbox(Inches(XI + 0.1), Inches(YI + 0.05), Inches(2.6), Inches(0.95))
f_testo(lb, [("Macchina virtuale — Microsoft Azure", 9, GREY_T, True),
             ("di proprietà della Fondazione (a canone)", 8, GREY_M, False),
             ("accesso via VPN: manutenzione, schema e tabelle", 8, GREY_M, False)],
        anchor=MSO_ANCHOR.TOP, margine=0.03)
wa = f_box(XI + 2.75, YI + 0.25, 1.1, 0.6, [("Web app Infor", 8.5, GREY_T, True), ("ASPX + JavaScript", 7.5, GREY_M, False)],
           line=COL["Infor"])
db = f_box(XI + 4.0, YI + 0.25, 1.1, 0.6, [("SQL Server", 8.5, GREY_T, True), ("tabelle referenziate", 7.5, GREY_M, False)],
           line=COL["Infrastruttura"])
f_freccia(XI + 3.85, YI + 0.55, XI + 4.0, YI + 0.55)
# passo 4 -> DB
f_freccia(box[3].left / 914400 + W / 2, Y + H, XI + 4.55, YI + 0.25, gomito=True)

# Inserimento nel deck ---
ids = prs.slides._sldIdLst
nuova = list(ids)[-1]
ids.remove(nuova); ids.insert(POSIZIONE - 1, nuova)
prs.save(FILE)
print(f"Slide inserita in posizione {POSIZIONE} in {FILE.name}")
