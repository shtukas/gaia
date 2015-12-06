
// --------------------------------------------
// -- CLASSIC SEARCH SUPPORT
// --------------------------------------------

var true_if_an_evaluation_is_running = false;
var last_command_line_contents = null;

function unique(array){
	return array.filter(function(el, index, arr) {
		return index === arr.indexOf(el);
	});
}

function cycle_command_line_classic_server_evaluation(line){

	var search_start_time = (new Date()).getTime();
	true_if_an_evaluation_is_running = true;
	last_command_line_contents = line;
		$("#div-id-evaluation-speed").html( '---------------' )

	$("#div-id-evaluation-speed").html(  ((new Date()).getTime() - search_start_time)/1000 + " seconds" )
	$("#div-id-evaluation-result-display").html('<div class="unit-dotted-border">You are searching: '+line+'</div>')
	true_if_an_evaluation_is_running = false;
	if($("#div-id-command-line-classic-input-text").val().trim()!=last_command_line_contents){
		cycle_command_line_classic_processing();
	}

	return;

	true_if_an_evaluation_is_running = true;
	last_command_line_contents = line;
	$("#div-id-evaluation-speed").html( '---------------' );
	var search_start_time = (new Date()).getTime();
	$.ajax({
		type: "GET",
		url: '/api/search1/'+encodeURIComponent(line),
		data: null,
		success: function(html){
			$("#div-id-evaluation-speed").html(  ((new Date()).getTime() - search_start_time)/1000 + " seconds" )
			$("#div-id-evaluation-result-display").html('<div class="unit-dotted-border">'+html+'</div>')
			true_if_an_evaluation_is_running = false;
			if($("#div-id-command-line-classic-input-text").val().trim()!=last_command_line_contents){
				cycle_command_line_classic_processing();
			}
		},
		error: function(){
			true_if_an_evaluation_is_running = false;
		}
	});
}

function cycle_command_line_classic_processing(){
	var line = $("#div-id-command-line-classic-input-text").val().trim();
	cycle_command_line_classic_server_evaluation(line);
}

$(document).ready(function(){
	$("#div-id-command-line-classic-input-text").on('input',function(){
		if(!true_if_an_evaluation_is_running) {
			cycle_command_line_classic_processing();
		}
	});
});

// --------------------------------------------
// -- DOCUMENT READY
// --------------------------------------------

$(document).ready(function(){
	$("#div-id-communication-timeline-input-text").on('input',function(){
		if(!true_if_an_evaluation_is_running) {
			cycle_communication_timeline_processing();
		}
	});
});

