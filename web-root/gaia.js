
// --------------------------------------------
// -- CLASSIC SEARCH 
// --------------------------------------------
/*
	(api_v1_search_dataset) = 
		[
			{
				"location": "/Users/pascal/Desktop/pascal-.txt",
				"caskey": "a82e0c6ac0bf958fadbab48abc6345e0b6e83393"
			}, 
			{
				"location": "/Users/pascal/Desktop/pascal-news-entry.txt",
				"caskey": "f02cb0a2c02fcfe3c85e796e559f3ad059ff2798"
			}
		]
*/

var true_if_an_evaluation_is_running = false;
var last_command_line_contents = null;

function unique(array){
	return array.filter(function(el, index, arr) {
		return index === arr.indexOf(el);
	});
}

function api_v1_search_dataset_to_html(dataset){
	return dataset
			.map(function(element){
				return '<li><a href="/api/v1/aion-point/'+element.caskey+'">'+element.location+'</a></li>'
			})
			.join('')
}

function cycle_command_line_classic_server_evaluation(line){
	true_if_an_evaluation_is_running = true;
	last_command_line_contents = line;
	$("#div-id-evaluation-speed").html( '---------------' );
	var search_start_time = (new Date()).getTime();
	$.ajax({
		type: "GET",
		url: '/api/v1/search/'+encodeURIComponent(line),
		data: null,
		success: function(dataset){
			$("#div-id-evaluation-speed").html(  ((new Date()).getTime() - search_start_time)/1000 + " seconds" )
			$("#div-id-evaluation-result-display").html('<div class="unit-dotted-border">'+api_v1_search_dataset_to_html(dataset)+'</div>')
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
	if(line.length<3){
		return;
	}
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

