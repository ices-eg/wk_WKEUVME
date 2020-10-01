

# get data from ICES sharepoint --> WKEUVME

  # now create different folders to store all VMS data 
  dir.create(paste(pathdir_nogit,"VMS data repository",sep="/"))
  fisheries_path <- paste(pathdir_nogit,"VMS data repository",sep="/")
  
  fnames <- spfiles("2020 Meeting Docs/06. Data/Technical workshop files/Fisheries Data", full = TRUE)
  for (fname in fnames) {
    spgetfile(fname, destdir = fisheries_path)
  }
  
  # now create different folders to store all VME data
  dir.create(paste(pathdir_nogit,"VME data repository",sep="/"))
  dir.create(paste(pathdir_nogit,"VME data repository/VME observations and csquares",sep="/"))
  VME_path <- paste(pathdir_nogit,"VME data repository/VME observations and csquares",sep="/")
  
  fnames <- spfiles("2020 Meeting Docs/06. Data/Technical workshop files/VME Data", full = TRUE)
  for (fname in fnames) {
    spgetfile(fname, destdir = VME_path)
  }
  
  dir.create(paste(pathdir_nogit,"VME data repository/VME elements",sep="/"))
  VME_element_path <- paste(pathdir_nogit,"VME data repository/VME elements",sep="/")
  
  fnames <- spfiles("2020 Meeting Docs/06. Data/Technical workshop files/VME elements", full = TRUE)
  for (fname in fnames) {
    spgetfile(fname, destdir = VME_element_path)
  }
  
  # SP_clearpassword() # which just calls 
  keyring::key_delete("icesSharePoint", ices_username)
  
  
  
  