# PDF - pdf library for reading and writing pdf files

Work in progress, but may load and save simple pdf files.

* PDF = pdf:load("file.pdf")  %% return a pdf document as map
* pdf:save("file.pfd", PDF).  %% save pdf file
* pdf:info(PDF).              %% return pdf info
* pdf:text(PDF).              %% extract pdf text
* pdf:pages(PDF).             %% return list of page object references

Images and Fonts still need to be worked on, and
also various encode/decode formats are currently missing.

It do parse "CompactedPDFSyntaxTest.pdf" so the basic parse
do handle some corner cases.


