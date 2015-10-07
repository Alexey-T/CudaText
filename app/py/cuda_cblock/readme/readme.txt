Plugin for CudaText.
Select several lines (no need to select full lines). Plugin menuitem will convert
these lines into C-block: adds indent (indent string depends on app settings)
to each line and surrounds block with { } brackets (indent for brackets is taken
from 1st selected line).

From:
           line1
		   line2
		     line3
		   line4
		   
To:
           {
	           line1
			   line2
			     line3
			   line4
           }

Author: Alexey T
