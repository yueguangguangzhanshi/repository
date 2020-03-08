library(shiny)
library(mailR)
library(shinyAce)
ui <- pageWithSidebar(
  
  headerPanel("Email sender"),
  
  sidebarPanel(
    textInput("from", "From:", value="from@gmail.com"),
    textInput("to", "To:", value="to@gmail.com"),
    textInput("subject", "Subject:", value=""),
    actionButton("send", "Send mail")
  ),
  mainPanel(    
    aceEditor("message", value="write message here")
  )
  
)

server <-shinyServer(function(input, output, session) {
  sendMail <- function(from0,to0,subject0, body0, attachFiles) {
    send.mail(from = from0,#发送者
      to = to0,
      #接受者，接受者可以使用多人，c("on@qq.com","two@qq.com")
      subject = subject0,
      #邮件主题
      body = body0,
      #邮件正文
      html = TRUE,
      encoding = "utf-8",
      smtp = list(
        host.name = "smtp.qq.com",
        port = 465,
        user.name = "471155858@qq.com",
        #发送者
        passwd = "mvecyssaucqicaad",
        #密码
        ssl = TRUE,
        tls = TRUE
      ),
      authenticate = TRUE,
      attach.files = attachFiles,
      #附件
      send = TRUE
    )
  }
  observe({
    if(is.null(input$send) || input$send==0) return(NULL)
    from <- isolate(input$from)
    to <- isolate(input$to)
    subject <- isolate(input$subject)
    msg <- isolate(input$message)
    sendMail(from, to, subject, msg,NULL)
  })
  
})

# server <- function(input, output) {
#   InfromMe <- function(info){
#     from <- "471155858@qq.com"
#     to <- "liuhebin.1019@163.com"
#     subject <- info$subject
#     body <- info$body                    
#     mailControl=list(smtpServer="smtp.qq.com",smtpPort=465)
#     sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)
#   }
#   observe({
#     # Take a dependency on input$goButton
#     if (input$goButton == 0)
#       return(NULL)
#     # Use isolate() to avoid dependency on input$goButton
#     isolate({
#       info <- data.frame(subject=paste("New info from:",input$name),
#                          body = info$body)
#       InfromMe(info)
#     })
#   })
# }
runApp(list(ui=ui,server=server))