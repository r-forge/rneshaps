
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> No content added. </p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>


<b>Software Abstract<br>
May 15, 2013
</b>
<br><br>LLNL-CODE-637312
<DL>
<DT>1a.	Identification
<DD>acronym: Rneshaps<br>
short: Functions for working with CAP88-PC version 1 concentration output files
<DT>2.	Author name and affiliation
<DD>Donald H. MacQueen, Lawrence Livermore National Laboratory
<DT>3.	Software completion date:
<DD>Original code spring 2009; current package version spring 2013.
<DT>4.	Brief description
<DD>This software is used to combine output files from US EPA CAP88-PC software (http://www.epa.gov/radiation/assessment/CAP88/aboutcap88.html) and estimate input parameters for CAP88-PC. Also includes features for tabular and graphical review of results.
<DT>5.	Method of solution
<DD>The software uses industry-standard nonlinear optimization routines to search for input parameters that provide a best fit of model output to data.
<DT>6.	Computer(s) for which software is written
<DD>Linux, Mac OS, Windows
<DT>7.	Operating system:
<DD>Any current or recent version of Linux, Mac OS, or Windows
<DT>8.	Programming Languages used:
<DD>R (95%), FORTRAN (5%)
<DT>9.	Software limitations
<DD>Available RAM, but this limit is unlikely to be encountered in typical usage. Software has been tested only on Mac OS, but R is multi-platform, so this software is expected to function on all platforms supported by R, provided users meet the requirements listed in item 11.
<DT>10.	Unique features of the software
<DD>Designed specifically for CAP88-PC output files, and for estimating CAP88-PC input parameters.
<DT>11.	Related and auxiliary software
<DD>Users must have R software and a compatible FORTRAN compiler.
<DT>12.	Other programming or operating information or restrictions
<DD>R software is licensed under GPL 2, so users must comply with GPL 2 license terms.
<DT>13.	Hardware requirements
<DD>Must support any recent Linux, Macintosh, or Windows operating system.
<DT>14.	Time requirements
<DD>Longest run time encountered to date is a few minutes, depending on what element of the software package is being used.
<DT>15.	References
<DD>
(group 1)<br>
A Best Fit Approach to Estimating Multiple Diffuse Source Terms Using Ambient Air Monitoring Data and an Air Dispersion Model, LLNL-JRNL-608237-DRAFT; accepted for publication in Operational Radiation Safety, March 2013.
<br><br>
(group 2)<br>
R Core Team. R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0. URL http://www.R-project.org/; 2012.
<br>
U.S. Environmental Protection Agency. User's guide for CAP88-PC Version 1. EPA 402-B-92-001; 1992. Available at http://www.epa.gov/rpdweb00/docs/cap88/402-b-92-001.pdf. Accessed 20 December 2012.
</DL>


</body>
</html>
