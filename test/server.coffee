net = require('net')

server = net.createServer (socket) ->
  socket.on 'data', (data) ->
    [action, args...] = JSON.parse data.toString()

    console.log action
    console.log args

    # switch action
    #   when 'log'
    #     console.log.apply console, args

server.listen(process.env.PORT, '127.0.0.1')
