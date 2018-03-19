library(telegram.bot)
library(mongolite)

#Bot config and log into DB
updater <<- Updater(token=bot_token('ToDoBot'))
dispatcher <- updater$dispatcher

#Name of the database set as a constant
DATABASE <- "TelegramBot"

#Commands functions
#Kill Bot Function
# kill <- function(bot, update){
#   bot$sendMessage(chat_id = update$message$chat_id, text = "Bye!")
#   # Clean 'kill' update
#   bot$getUpdates(offset = update$update_id + 1)
#   # Stop the updater polling
#   updater$stop_polling()
# }

start <- function(bot, update) {
  bot$sendMessage(chat_id = update$message$chat_id, text = "Bienvenido al ToDo Bot en español. Escribe \"/\" para ver los comandos disponibles")
}

#Function to show today task for the user
showTodayTask <- function(bot, update) {
  collect <- mongo(collection = paste("today", update$message$from$username, sep="-"), db = DATABASE)
  if(collect$count() == 0) {
    bot$sendMessage(chat_id = update$message$chat_id, text = "No existen tareas hoy para este usuario")
  } else {
    elems <- collect$find('{}')
    for (i in 1:collect$count()) {
      bot$sendMessage(chat_id = update$message$chat_id, text = elems[i,1])
    }
  }
}

#Function to set a task today for the user
setTodayTask <- function(bot, update, args) {
  collect <- mongo(collection = paste("today", update$message$from$username, sep="-"), db = DATABASE)
  actualsize <- collect$count()
  data <- paste(args, collapse = ' ')
  query <- paste('{"task":"', data, '"}')
  collect$insert(query)
  postsize <- collect$count()
  if (postsize == actualsize + 1) {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Se ha añadido su tarea para hoy.")
  } else {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Ha habido problemas cargando su tarea en la base de datos")
  }
}

#Function to complete a task for a user
deleteTodayTask <- function(bot, update, args) {
  collect <- mongo(collection = paste("today", update$message$from$username, sep="-"), db = DATABASE)
  actualsize <- collect$count()
  data <- paste(args, collapse = ' ')
  query <- paste('{"task":"', data, '"}')
  collect$remove(query)
  postsize <- collect$count()
  if (postsize == actualsize - 1) {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Se ha completado su tarea para hoy.")
  } else {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Ha habido problemas borrando su tarea en la base de datos")
  }
}

#Function to show tomorrow task for the user
showTomorrowTask <- function(bot, update) {
  collect <- mongo(collection = paste("tomorrow", update$message$from$username, sep="-"), db = DATABASE)
  if(collect$count() == 0) {
    bot$sendMessage(chat_id = update$message$chat_id, text = "No existen tareas mañana para este usuario")
  } else {
    elems <- collect$find('{}')
    for (i in 1:collect$count()) {
      bot$sendMessage(chat_id = update$message$chat_id, text = elems[i,1])
    }
  }
}

#Function to set a task tomorrow for the user
setTomorrowTask <- function(bot, update, args) {
  collect <- mongo(collection = paste("tomorrow", update$message$from$username, sep="-"), db = DATABASE)
  actualsize <- collect$count()
  data <- paste(args, collapse = ' ')
  query <- paste('{"task":"', data, '"}')
  collect$insert(query)
  postsize <- collect$count()
  if (postsize == actualsize + 1) {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Se ha añadido su tarea para mañana.")
  } else {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Ha habido problemas cargando su tarea en la base de datos")
  }
}

#Function to complete a task tomorrow for a user
deleteTomorrowTask <- function(bot, update, args) {
  collect <- mongo(collection = paste("tomorrow", update$message$from$username, sep="-"), db = DATABASE)
  actualsize <- collect$count()
  data <- paste(args, collapse = ' ')
  query <- paste('{"task":"', data, '"}')
  collect$remove(query)
  postsize <- collect$count()
  if (postsize == actualsize - 1) {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Se ha completado su tarea para mañana.")
  } else {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Ha habido problemas borrando su tarea en la base de datos")
  }
}

#Function in case the command is not supported
unknown <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "No te entiendo :(")
}

#Handlers configuration
# kill_handler <- CommandHandler('kill', kill)
# dispatcher$add_handler(kill_handler)

start_handler <- CommandHandler('start', start)
dispatcher$add_handler(start_handler)

showToday_handler <- CommandHandler('TareasHoy', showTodayTask)
dispatcher$add_handler(showToday_handler)

setToday_handler <- CommandHandler('AgregarTareaHoy', setTodayTask, pass_args = TRUE)
dispatcher$add_handler(setToday_handler)

deleteToday_handler <- CommandHandler('CompletarTareaHoy', deleteTodayTask, pass_args = TRUE)
dispatcher$add_handler(deleteToday_handler)

showTomorrow_handler <- CommandHandler('TareasManana', showTomorrowTask)
dispatcher$add_handler(showTomorrow_handler)

setTomorrow_handler <- CommandHandler('AgregarTareaManana', setTomorrowTask, pass_args = TRUE)
dispatcher$add_handler(setTomorrow_handler)

deleteTomorrow_handler <- CommandHandler('CompletarTareaManana', deleteTomorrowTask, pass_args = TRUE)
dispatcher$add_handler(deleteTomorrow_handler)

unknown_handler <- MessageHandler(unknown, Filters$command)
dispatcher$add_handler(unknown_handler)

#Start Listening for messages
updater$start_polling()
