<?php
   $results = array(
   				"results" => array(
   						array("result" => "10.8-Kampong Cham", "confidence" => 10.8),
   						array("result" => "9.8-Phnom Penh", "confidence" => 9.8),
   						array("result" => "7.8-Pursat", "confidence" => 7.8),
   						// array("result" => "5.8-Kompot", "confidence" => 5.8)
   				),

   				"error"  => ""
   		    );

   echo json_encode($results);
