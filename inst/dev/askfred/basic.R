# scrape ref: https://freakonometrics.hypotheses.org/83057
fred_results <- "https://www.askfred.net/results"
download.file(fred_results, "tempFred.html")

f_tab <- XML::readHTMLTable(fred_results)
# XML content does not seem to be XML:

f_web <- RCurl::getURL(fred_results)
# Error in function (type, msg, asError = TRUE)  : 
#   schannel: CertGetCertificateChain trust error CERT_TRUST_IS_UNTRUSTED_ROOT

html_path <- xml2::download_html(
  fred_results, file = tempfile(fileext = ".html"))
