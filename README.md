the process_origin script relies on two reference items, stored in the reference folder:

1) ESBasic_ERM_Desc.xlsx - this is the template that is used to write data into an EDD of the specified format

2) origins_remap_guide - this contains remapping information for each set of reference values and can be updated as needed.
   
when processing this data, an important QC check is to be sure no cas_rn or lab_anl_method fields are blank- if they are this is typically because
there is a new reference value that has not yet been added to the remap guide.

this script will export two files into the output folder: an EDD and a QC reference items file. this is a summary of any values that underwent remapping
or location id/parent sample code/task code assignment so that these can be reviewed/QCd before loading.

