var initVoice = function() {
if (annyang) {
  inputFunc = function(command, func) {
      // alert(command);
      Shiny.onInputChange('command', func.concat(' ', command));
  };
  arrange = function(command) {
      inputFunc(command, 'arrange');
  };
  filter = function(command) {
      inputFunc(command, 'filter');
  };
  not_filter = function(command) {
      inputFunc(command, 'filter out');
  };

  select = function(command) {
      inputFunc(command, 'select');
  };
  not_select = function(command) {
      inputFunc(command, 'select out');
  };

  group_by = function(command) {
      inputFunc(command, 'group_by');
  };
  count = function(command) {
      inputFunc(command, 'count');
  };
  add_count = function(command) {
      inputFunc(command, 'add_count');
  };
  tally = function() {
      inputFunc('', 'tally');
  };
  add_tally = function() {
      inputFunc('', 'add_tally');
  };

  replace = function(command) {
      inputFunc(command, 'replace');
  };
  plot = function(command) {
      inputFunc(command, 'ggplot');
  };
  reset = function() {
      inputFunc('', 'reset');
  };
  undo = function() {
    var retstring = "undo ".concat(Math.random());
    inputFunc('', retstring);
  };

  regress = function(yvar, command) {
    var retstring = yvar.concat(' on ', command);
    inputFunc(retstring, 'regress');
  };

  var commands3 = {
    'sort *command': arrange,
    'arrange *command': arrange,
    'order *command': arrange,
    'group by *command': group_by,
    'group the data by *command': group_by,
    'group the data set by *command': group_by,
    'group data by *command': group_by,
    'group *command': group_by,

    'regress(ed) :yvar on *command': regress,
    'progress(ed) :yvar on *command': regress,


    'add count *command': add_count,
    'add counts *command': add_count,

    'count by *command': count,
    'count up *command': count,
    'count *command': count,
    'table *command': count,

    'subset row *command': filter,
    'subset rows *command': filter,
    'select row *command': filter,
    'select rows *command': filter,
    'grab row *command': filter,
    'grab rows *command': filter,
    'keep row *command': filter,
    'keep rows *command': filter,
    'drop row *command': not_filter,
    'drop rows *command': not_filter,
    'remove row *command': not_filter,
    'remove rows *command': not_filter,

    'replace *command': replace,
    'make *command': replace,
    'coerce *command': replace,
    'change *command': replace,

    'subset column *command': select,
    'subset columns *command': select,
    'select column *command': select,
    'select columns *command': select,
    'grab column *command': select,
    'grab columns *command': select,
    'keep column *command': select,
    'keep columns *command': select,
    'drop column *command': not_select,
    'drop columns *command': not_select,
    'remove column *command': not_select,
    'remove columns *command': not_select,

    'g g plot *command': plot,
    'ggplot *command': plot,
    'plot *command': plot,
    'pot *command': plot,

    'subset rose *command': filter,
    'select rose *command': filter,
    'grab rose *command': filter,
    'keep rose *command': filter,
    'drop rose *command': not_filter,
    'remove rose *command': not_filter,

    'filter *command': filter,
    'select *command': select,

    'tally': tally,
    'add tally': add_tally,


    'undo again': undo,
    'undo': undo,
    'undue': undo,
    'reset': reset
  } ;
  var commands = {
    'title *title': function(title) {
      Shiny.onInputChange('title', title);
    },
    'color :color': function(color) {
      Shiny.onInputChange('color', color);
    },
    'bigger': function() {
      bigger += 1;
      Shiny.onInputChange('bigger', bigger);
    },
    'smaller': function() {
      if (bigger >= 1.5) {
        bigger -= 1;
        Shiny.onInputChange('bigger', bigger);
      }
    },
    'regression': function() {
      Shiny.onInputChange('yes', Math.random());
    }
  };
  // annyang.addCommands(commands);
  // annyang.addCommands(hello_commands);
  annyang.addCommands(commands3);
  annyang.start();
  annyang.addCallback('result', function(phrases) {
    console.log("I think the user said: ", phrases[0]);
    // Shiny.onInputChange('phrase', phrases[0]);
    Shiny.onInputChange('phrase', phrases);
    console.log("But then again, it could be any of the following: ",
    phrases);
  });

  }

};

$(function() {
  setTimeout(initVoice, 10);
});

