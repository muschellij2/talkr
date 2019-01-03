var initVoice = function() {
if (annyang) {
  // var bigger = 1;
  Shiny.onInputChange('command', 'say command something');
  Shiny.onInputChange('yes', 'no');
  var commands = {
    // 'arrange': function(cmd) {  
    'command *command': function(command) {
      Shiny.onInputChange('command', command);
  	}
    // },
    // 'color :color': function(color) {
    //   Shiny.onInputChange('color', color);
    // },
    // 'bigger': function() {
    //   bigger += 1;
    //   Shiny.onInputChange('bigger', bigger);
    // },
    // 'smaller': function() {
    //   if (bigger >= 1.5) {
    //     bigger -= 1;
    //     Shiny.onInputChange('bigger', bigger);
    //   }
    // },
    // 'regression': function() {
    //   Shiny.onInputChange('yes', Math.random());
    // }
  };
  annyang.addCommands(commands);
  annyang.start();
  }
};

$(function() {
  setTimeout(initVoice, 10);
});
