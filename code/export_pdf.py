# export_pdf.py: print the revealjs deck to PDF with Playwright (fallback when Chrome headless returns a blank file)
import sys, asyncio
from playwright.async_api import async_playwright
html, pdf = sys.argv[1], sys.argv[2]
async def main():
    async with async_playwright() as p:
        b = await p.chromium.launch(); pg = await b.new_page(viewport={"width": 1050, "height": 700})
        await pg.goto("file://" + html + "?print-pdf", wait_until="load")
        await pg.wait_for_timeout(10000)
        await pg.pdf(path=pdf, prefer_css_page_size=True, print_background=True)
        await b.close()
asyncio.run(main())
print("PDF written:", pdf)
