####
#### Praat script ParseValidateVersion.praat
#### Dan Villarreal (d.vill@pitt.edu)
#### Version date: 1 Apr 2024
####
#### Contains two procedures that are useful for other scripts to ensure Praat
#### version meets minimum requirements (e.g., array functions introduced in 
#### Praat 6.1):
#### - parseVersion: Robustly parse Praat version string passed as argument
####                 into the triple (major,minor,patch) triple.
####                 If the version string is in the format "X.Y.Z", sets
####                 patch to 0.
#### - validateVersion: Compare current Praat version to minimum version string
####                    passed as argument, returning error if current version
####                    is below minimum version
####

procedure parseVersion: .v$
  ##Ensure version string is correctly formatted
  versionRE$ = "^\d+\.\d+(\.\d+)?$"
  if not index_regex(.v$, versionRE$)
    exitScript: "Error in @parseVersion: Incorrect format for version string (", .v$, ")."
  endif
  
  ##Parse version string
	.major = number(replace_regex$(.v$, "\..+", "", 0))
	dot1 = index(.v$, ".")
	dot2 = rindex(.v$, ".")
	if dot1 = dot2
		.minor = number(replace_regex$(.v$, ".+\.", "", 0))
		.patch = 0
	else
		noMajor$ = replace_regex$(.v$, "^.+?\.", "", 0)
		.minor = number(replace_regex$(noMajor$, "\..+", "", 0))
		.patch = number(replace_regex$(.v$, ".+\.", "", 0))
	endif
endproc

procedure validateVersion: .minVersion$
  ##Parse current and minimum versions
	@parseVersion: praatVersion$
	currMajor = parseVersion.major
	currMinor = parseVersion.minor
	currPatch = parseVersion.patch
	@parseVersion: .minVersion$
	minMajor = parseVersion.major
	minMinor = parseVersion.minor
	minPatch = parseVersion.patch
  
  ##Construct exit message
  exitMsg$ = "This script requires Praat to be at least version " + .minVersion$ + " (you have version " + praatVersion$ + "). Please download a more recent version of Praat at https://www.fon.hum.uva.nl/praat/."
  
  ##Compare current to minimum
  if currMajor < minMajor
    exitScript: exitMsg$
  elsif currMajor = minMajor
    if currMinor < minMinor
      exitScript: exitMsg$
    elsif currMinor = minMinor
      if currPatch < minPatch
        exitScript: exitMsg$
      endif
    endif
  endif
endproc
