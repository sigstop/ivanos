# Produce HTMLS from all Markdown files in a directory
# Lincoln Mullen | http://lincolnmullen.com | lincoln@lincolnmullen.com

# List files to be made by finding all *.md files and appending .pdf
HTMLS := $(patsubst %.md,%.html,$(wildcard *.md))
DEST = ../../priv/www/

# The all rule makes all the PDF files listed
all : $(HTMLS)
                        
# This generic rule accepts PDF targets with corresponding Markdown 
# source, and makes them using pandoc
%.html : %.md
	markdown.pl $< > ../../priv/www/$@

# rest of this ain't required, the make script just clobbers everything
# every time for some reason...
#
# # Remove all PDF outputs
# clean :
# 	rm $(HTMLS)
# 
# # Remove all PDF outputs then build them again
# rebuild : clean all

