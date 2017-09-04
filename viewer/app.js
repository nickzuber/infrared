'use strict'

const express = require('express')
const path = require('path')

const _port = 8080

const app = express()

// Allow access to file directory
app.use(express.static(path.join(__dirname, 'scripts')))

// Set up routes
app.get('/', (req, res) => res.sendFile(`${__dirname}/views/index.html`))
app.get('*', (req, res) => res.send('404 error: page not found'))

// Create server
app.listen(_port, function(){
	console.log('Ready on port ' + _port + '...')
})

