#!/usr/bin/python3
# A simple script to upload a file with a CGI form

# Import modules for CGI handling 
import cgi, os
import cgitb; cgitb.enable()

# Create instance of FieldStorage 
form = cgi.FieldStorage()

# Get filename from field FILENAME
fileitem = form['FILENAME']

# Get desired upload file name from field UPLOADNAME
if form.getvalue('UPLOADNAME'):
   upname = form.getvalue('UPLOADNAME')
else:
   upname = "UNNAMED"

# Test if the file was uploaded
if fileitem.filename:
   # strip leading path from upload file name to avoid
   # directory traversal attacks
   upfn = 'uploads/' + os.path.basename(upname)
   open(upfn, 'wb').write(fileitem.file.read())
   message = 'File "' + fileitem.filename + '" successfully uploaded to "' + upfn + '"'
else:
   message = 'No file uploaded'

# Redirect to a URL if defined by field REDIRECT
if form.getvalue('REDIRECT'):
   print("Location: " + form.getvalue('REDIRECT'))
   print("")
else:
   print("Content-Type: text/html")
   print("")
   print("<html>")
   print("<body>")
   print(" <p>%s</p>" % (message,))
   print("</body>")
   print("</html>")
