################################
#
#  5.7 updated as of 19 FEB 2021
#  Author:  Paul Evangelista
#  Modeling various testing strategies 
#  to prevent and detect COVID within USCC
#
################################

use List::Util qw[shuffle];

#Bernoulli rv function
sub bern{
	my $p = $_[0];
	my $rn = rand();
	my $rv = 0;
	if($rn < $p){
		$rv = 1;
	}
	return $rv;
}

#Function to create new infection; populates @infected, assigns infection time, 
#assigns symptomatic condition (0 or 1)
sub new_infection{
	my @new_args = @_;
	my $ninf = $new_args[0];
	my $infect_time_s = $new_args[1];
	my $pos_infect = 0;
	if(exists($h_immune{$ninf}) || exists($infect_time{$ninf})){}
	else{
		push(@infected, $ninf);
		$infect_time{$ninf} = $infect_time_s;
		$sympto{$ninf} = bern($sympto_rate);
		$active_infection{$ninf} = 1;
		$pos_infect = 1;
		$total_infections++;
		count_group_inc($ninf);
		return($pos_infect);
	}
}

# increment infection count for company, team, and section
# of newly infected cadet
sub count_group_inc {
	my $ninf = $_[0]; #new_infection
	if (exists($h_team{$ninf})){
		my $team = $h_team{$ninf};
		$team_infect[$team]++;
	}
	my $company = $h_company{$ninf};
	$company_infect[$company]++;
	foreach my $section (@{$section_sched{$ninf}}){
		$n_section{$section} = $n_section{$section} + 1;
		$section_infect[$section]++;
	}
}

# decrement infection count for company, team, and section
# of recovered / isolated cadet
sub count_group_dec {
	my $ninf = $_[0]; #old_infection
	if (exists($h_team{$ninf})){
		my $team = $h_team{$ninf};
		$team_infect[$team]--;
	}
	my $company = $h_company{$ninf};
	$company_infect[$company]--;
	foreach my $section (@{$section_sched{$ninf}}){
		$n_section{$section} = $n_section{$section} + 1;
		$section_infect[$section]--;
	}
}


#testing function; increments TP/FN/TN/FP counts
sub test_cadet{
	my $cdt = $_[0];
	my $test_result = 0;
	if($active_infection{$cdt} == 1){
		$test_result = bern($tp_rate);
		if($test_result == 1){ $TP_count++;}
		else{$FN_count++;}
	}
	else{
		$test_result = bern(1 - $tn_rate);
		if($test_result == 0){$TN_count++;}
		else{
			$FP_count++;
			push(@quarantine,$remove); #important logic to capture FP tests in quarantine
			push(@quarantine_time,$i);
		}
	}
	$tested++;
	$ind_tested{$cdt} = 1;
	return($test_result);
}

###############################
############################### Critical parameters:

$test_qty = $ARGV[0]; #specify random number to test each week
$test_team = $ARGV[1]; #fraction of team to test
$test_company = $ARGV[2]; #fraction of company to test
$test_sections = $ARGV[3]; #fraction of section mates to test
$rr = $ARGV[4];	#avg number of cadets per room
$tp_rate = $ARGV[5]; #sensitivity
$tn_rate = $ARGV[6];  #specificity
$sympto_rate = $ARGV[7]; #fraction between 0 and 1 representing pct symptomatic
$company_lockdown_tol = $ARGV[8]; #number of cadets in isolation before lockdown enforced
$vis = $ARGV[9]; #changes output to support R visualization
  
$IT = 2;  #infectious time (IT)
$weeks_sim = 16; #sixteen weeks in a semester before cadets are sent home
$test_mult = 0;  #for a test multiplier based on number in quarantine
$initial_infection_count = 2; #number of cadets infected with COVID at the start of the simulation
# there are no other infection injects

$immune_start = 0; # nbr of immune cadets to start (880 for ~20%)
# this represents the number of immune cadets based upon previous infections
$testing_on = 1; #1 if testing active, 0 if no testing
$verbose = 0; #infection details print at bottom, 1 if yes, 0 if no

$max_q = 0;

# solve below with system of linear equations
# 4392 = 3*$R3 + 2*$R2
# 4392/$rr = $R3 + $R2
$R2 = 3*(122/$rr) - 122; #number of 2 man rooms per company
$R3 = 122/$rr - $R2;  #number of 3 man rooms per company
$R2 = int($R2);

##############################
##############################
$total_infections = $IT;
$TP_count = 0;
$TN_count = 0;
$FP_count = 0;
$FN_count = 0;

$ncadets = 4392;
$ncompanies = 36;
$cdts_per_company = 122;
$nrooms = 1760;
$nteams = 50;
$s_team = 50;
$n_sections = 1711;

$cdt_count = 0;
$room_nbr = 0;
$room_count = 0;

# this loop creates three hash assignments:  company, room, and class (0 for plebe/yrlng, 1 for cow/firstie)

for($i=0;$i<$ncompanies;$i++){ #company
	$company_infect[$i] = 0;
	$company_room_count = 0;
	$room_max = 2;
	for($j=0;$j<122;$j++){ #cdts in company
		$h_room{$cdt_count} = $room_nbr;
		push(@ {$a_room{$room_nbr}}, $cdt_count); 
		$h_company{$cdt_count} = $i;
		push(@ {$a_company{$i}}, $cdt_count);
		$room_count++;
		if($company_room_count > $R2){
			$room_max = 3;
		}
		if($j <=60) {
			if($room_count >= $room_max){
				$room_nbr++;
				$company_room_count++;
				$room_count = 0;
			}
			$h_class{$cdt_count} = 0;
			push(@{$a_class{1}}, $cdt_count);
		}
		if($j > 60){
			if($room_count >= $room_max){
				$room_nbr++;
				$company_room_count++;
				$room_count = 0;
			}
			$h_class{$cdt_count} = 1;
			push(@{$a_class{0}}, $cdt_count);
		}
		$cdt_count++;
	}
}


###########################
# assign every cadet to 7 sections
###########################

## loop below assigns all underclass to seven sections of class
$cdt_count = 0;
$section = 0;
for($i=0;$i<7;$i++){
	@cadets = shuffle(@{$a_class{0}});
	foreach my $c (@cadets){
		push(@{$section_sched{$c}}, $section);
		push(@{$section_list{$section}}, $c);
		$cdt_count++;
		if($cdt_count >= 18) {
			$cdt_count = 0;
			$section_infect[$section] = 0;
			$section++;
		}
	}
}
$section_infect[$section] = 0;
##loop below assigns all underclass to seven sections of class
$cdt_count = 0;
$section++;
for($i=0;$i<7;$i++){
	@cadets = shuffle(@{$a_class{1}});
	foreach my $c (@cadets){
		push(@{$section_sched{$c}}, $section);
		push(@{$section_list{$section}}, $c);
		$cdt_count++;
		if($cdt_count >= 18) {
			$cdt_count = 0;
			$section_infect[$section] = 0;
			$section++;
		}
	}
}  		
$section_infect[$section] = 0;
$total_sections = $section;

####################
# build 50 corps squad teams
####################

@under_class = @{$a_class{0}};
@upper_class = @{$a_class{1}};
@all_cadets = shuffle(@under_class, @upper_class);
$cdt_count = 0;

for($i=0;$i<50;$i++){
	$team_infect[$i] = 0;
	for($j=0;$j<50;$j++){
		push(@{$a_team{$i}}, $all_cadets[$cdt_count]);
		$h_team{$cdt_count} = $i;
		$cdt_count++;
	}
}

#########################
#  initial infections
#########################

@all_cadets = shuffle(@under_class, @upper_class);
$initial_infection_count--;
@infected = @all_cadets[0..$initial_infection_count];
foreach $infectee (@infected){
	$infect_time{$infectee} = 0;
	$sympto{$infectee} = bern($sympto_rate);
	count_group_inc($infectee);
}

$initial_infection_count++;
@a_immune = @all_cadets[$initial_infection_count..$immune_start];
foreach $cdt (@a_immune){
	$h_immune{$cdt} = 1;
}

##
# Simulate for $i weeks...
##

$size = @infected;

$roommate_infect = 0;
if($vis == 1){
	print "week,container,cont_element,infections,tested\n";
}
for($i=0;$i<$weeks_sim;$i++){
#for($i=0;$i<3;$i++){
	
	$company_hr = 6; #original 6
	$room_hr = 9; #original 9
	$team_hr = 5; #original 5
	$class_hr = 4; #original 4
	
	if($company_lockdown == 1){
		$company_hr = 15;
		$room_hr = 9;
		$team_hr = 0;
		$class_hr = 0;
	}

	## This needs to be examined; unit of measurement below is week, but we are 
	## carving up time as a proportion of a duty day...

	$company_rn = $company_hr/24;
	$room_rn = $company_rn + $room_hr/24;
	$team_rn = $room_rn + $team_hr/24;
	$class_rn = $team_rn + $class_hr/24;

	@curr_infected = @infected;
	@test_list = ();
	%ind_tested = ();
	%infectee_remove = ();
	$j=0;
	$tested = 0;
	foreach my $infectee (@curr_infected) {
		$inf_rn = rand();
		if($inf_rn < $company_rn){
			$company = $h_company{$infectee};
			@company_mates = shuffle(@{$a_company{$company}});
			if($company_mates[0] == $infectee){
				$company_mates[0] = $company_mates[1];	
			}
			$pos_infect = new_infection($company_mates[0],$i);
			if($pos_infect == 1) {
			}
		}
		if($inf_rn >= $company_rn && $inf_rn < $room_rn){
			$room_nbr = $h_room{$infectee};
			@company_mates = shuffle(@{$a_room{$room_nbr}});
			if($company_mates[0] == $infectee){
				$company_mates[0] = $company_mates[1];	
			}
			$pos_infect = new_infection($company_mates[0],$i);
			if($pos_infect == 1) {
				$roommate_infect++;
			}
		}
		if($inf_rn >= $room_rn && $inf_rn < $team_rn){
			if(exists($h_team{$infectee})){
				$team_nbr = $h_team{$infectee};
				@company_mates = shuffle(@{$a_team{$team_nbr}});
				$team_o_company = "team";
			}
			else{
				$team_nbr = $h_company{$infectee};
				@company_mates = shuffle(@{$a_company{$team_nbr}});
				$team_o_company = "company";
			}
			if($company_mates[0] == $infectee){
				$company_mates[0] = $company_mates[1];	
			}
			$pos_infect = new_infection($company_mates[0],$i);
			if($pos_infect == 1) {
				if($team_o_company eq "team"){
					#$team_infect[$team_nbr]++;
				}
				#else{$company_infect[$team_nbr]++;}
			}
		}
		if($inf_rn >= $team_rn){
			@section_list = shuffle(@{$section_sched{$infectee}});
			$section_inf = $section_list[0];
			@company_mates = shuffle(@{$section_list{$section_inf}});
			if($company_mates[0] == $infectee){
				$company_mates[0] = $company_mates[1];	
			}
			$pos_infect = new_infection($company_mates[0],$i);
			if($pos_infect == 1) {
				#print "$infectee inf. section $section_infect ...cadet $company_mates[0]\n";
				#$section_infect[$section_inf]++;
			}
		}
		if(($i - $infect_time{$infectee}) >= $IT){
			$remove = splice(@infected,$j,1);
			$h_immune{$infectee} = 1;
			$active_infection{$infectee} = 0;
			#print "\n1start: $company_infect[$h_company{$remove}]\n";
			count_group_dec($remove);
			#print "1fin: $company_infect[$h_company{$remove}]\n";
			$j--;
		}
		#####################
		##symptomatic testing
		#####################
		if($sympto{$infectee} == 1 && $active_infection{$infectee} == 1){
			push(@test_list,$infectee);
		}
		$j++;
	}
	@o_company_infect = @company_infect;
	@o_team_infect = @team_infect;
	@o_section_infect = @section_infect;

	####################
	## random testing
	####################
	
	## build stratified array of cadets
	$k=0;
	@seq_c = (0..121);
	for($ii=0;$ii<36;$ii++){
		@seq_c = shuffle(@seq_c);
		for($j=0;$j<122;$j++){
			$k = $seq_c[$j] + $ii*121;
			$idx = $j + $ii*121;
			$r_cdt_arr[$idx] = $k; #array of cadets stratified by company
			#$co_cdt[$ii][$j] = $k;
		}
	}
	
	$total_tested = $TP_count+$FN_count+$TN_count+$FP_count;
	#print "total tested start $i $total_tested\n";
	#@test_cadets = shuffle(@all_cadets); #no stratification
	@test_cadets = @r_cdt_arr;
	$test_qty_count = 0;
	$l_test_cadets = @test_cadets;
	for($m=0;$m<$l_test_cadets;$m++){
		$cdt = $test_cadets[$m];
		if($test_qty_count >= $test_qty){
			$m = $l_test_cadets;
			#print "$i $test_qty_count done\n";
		}
		else{
			if($h_immune{$cdt} == 1 || $quar_exp{$cdt} == 1){} #this assumes we know immune cadets
			else{
				$curr_test_result = test_cadet($cdt);
				$test_qty_count++;
				if($curr_test_result == 1){
					$curr_test_result = test_cadet($cdt); #test random pos cadets twice to confirm
					if($curr_test_result == 1){
						$infectee_remove{$cdt} = 1;
						push(@test_list,$cdt);
					}
						
				}
			}
		}
	}		
	$total_tested = $TP_count+$FN_count+$TN_count+$FP_count;

	######################
	#
	#  Team and Company Testing
	#
	#######################
	%section_tested = ();
	%team_tested = ();
	%company_tested = ();
	foreach my $infectee (@test_list){
		if($h_immune{$infectee} == 1 || $ind_tested{$infectee} == 1){}
		else{
			$curr_test_result = test_cadet($infectee);
			if($curr_test_result == 1){
				$infectee_remove{$infectee} = 1;
			}
		}
		if($test_team > 0){
			if(exists($h_team{$infectee}) && $team_tested{$h_team{$infectee}} != 1){
				$team_nbr = $h_team{$infectee};
				@company_mates = shuffle(@{$a_team{$team_nbr}});
				$l_company_mates = @company_mates;
				$t_l_company_mates = int($test_team*$l_company_mates + 0.5) - 1;
				@company_mates = @company_mates[0..$t_l_company_mates];
				foreach $mate (@company_mates){
					if($h_immune{$mate} == 1 || $ind_tested{$mate} == 1){}
					else{
						$curr_test_result = test_cadet($mate);
						if($curr_test_result == 1){
							$infectee_remove{$mate} = 1;
						}
					}
				}
				$team_tested{$team_nbr} = 1;
			}
		}
		$team_nbr = $h_company{$infectee};
		if($test_company > 0 && $company_tested{$team_nbr} != 1) {
			@company_mates = shuffle(@{$a_company{$team_nbr}});
			$l_company_mates = @company_mates;
			$t_l_company_mates = int($test_company*$l_company_mates + 0.5) - 1;
			@company_mates = @company_mates[0..$t_l_company_mates];
			foreach $mate (@company_mates){
				if($h_immune{$mate} == 1 || $ind_tested{$mate} == 1){}
				else{
					$curr_test_result = test_cadet($mate);
					if($curr_test_result == 1){
						$infectee_remove{$mate} = 1;
					}
				}
			}
			$company_tested{$team_nbr} = 1;
		}
		if($test_sections > 0){
			@section_list = @{$section_sched{$infectee}};
			@company_mates = ();
			foreach $section (@section_list){
				if($section_tested{$section} != 1){
					push(@company_mates,@{$section_list{$section}});
					$section_tested{$section = 1};
				}
			}
			@company_mates = shuffle(@company_mates);
			$l_company_mates = @company_mates;
			$t_l_company_mates = int($test_sections*$l_company_mates + 0.5) - 1;
			@company_mates = @company_mates[0..$t_l_company_mates];
			foreach $mate (@company_mates){
				if($h_immune{$mate} == 1 || $ind_tested{$mate} == 1){}
				else{
					$curr_test_result = test_cadet($mate);
					if($curr_test_result == 1){
						$infectee_remove{$mate} = 1;
					}
				}
			}
		}
	}
	$j=0;
	@current_infected = @infected;
	foreach $infectee (@current_infected){
		if($infectee_remove{$infectee} == 1){
			$remove = splice(@infected,$j,1);
			push(@quarantine,$remove);
			push(@quarantine_time,$i);
			$quar_exp{$infectee} = 1;
			count_group_dec($remove);
			$j--;
		}
		$j++;
	}
	if($company_lockdown_tol < $size_q){
		$company_lockdown = 1;
	}
	else{
		$company_lockdown = 0;
	}
	
	@c_quarantine = @quarantine;
	$j=0;
	foreach $infectee (@c_quarantine){
		if(($i - $quarantine_time[$j]) <= $IT){}
		else{
			$h_immune{$quarantine[$j]} = 1;
			$active_infection{$quarantine[$j]} = 0;
			$remove = splice(@quarantine,$j,1);
			$remove_t = splice(@quarantine_time,$j,1);
			$j--;
		}
		$j++;
	}
	
	if($verbose == 1){ 
		print "$i,$size_inf,$tested,$total_infections\n";	
	}
	$size_inf = @infected;
	$size_q = @quarantine;
	if($size_q > $max_q){
		$max_q = $size_q;
	}
	if($vis == 1){
		print "$i,meta,$tested,$size_q,0\n"; #last element is a dummy variable; can use for something
		$m=0;
		foreach my $j (@o_company_infect){
			if(exists($company_tested{$m})){}
			else{$company_tested{$m}=0;}
			print "$i,company,$m,$j,$company_tested{$m}\n";
			$m++;
		}
		$m=0;
		foreach my $j (@o_team_infect){
			if(exists($team_tested{$m})){}
			else{$team_tested{$m}=0;}
			print "$i,team,$m,$j,$team_tested{$m}\n";
			$m++;
		}
		$m=0;
		foreach my $j (@o_section_infect){
			if(exists($section_tested{$m})){}
			else{$section_tested{$m}=0;}
			print "$i,section,$m,$j,$section_tested{$m}\n";
			$m++;
		}
	}
}	
$total_tested = $TP_count+$FN_count+$TN_count+$FP_count;
if($vis == 0){
	print "$test_qty,$test_team,$test_company,$test_sections,$rr,$tp_rate,$tn_rate,$sympto_rate,$total_infections,$total_tested\n"; #without company_lockdown_tol
}
if($vis == 1){
	$summary_output = join(";",$test_qty,$test_team,$test_company,$test_sections,$rr,$tp_rate,$tn_rate,$sympto_rate,$company_lockdown_tol,$total_infections,$total_tested);
	print "$i,summary,$summary_output,0,0\n";
}


if($verbose == 1) {			

	print "TP FN TN FP, resp:  $TP_count $FN_count $TN_count $FP_count \n";

	sub print_infections{
		my @group = @_;
		@group = sort {$b <=> $a} @group;
		foreach $infect_count (@group){
			print "$infect_count ";
		}
		print "\n";
	}

	print "company infect counts: \n";
	print_infections(@company_infect);

	print "team infect counts: \n";
	print_infections(@team_infect);
	
	print "section infect counts: \n";
	print_infections(@section_infect);

	print "\n";
	print "roomate infections: $roommate_infect\n";
}		