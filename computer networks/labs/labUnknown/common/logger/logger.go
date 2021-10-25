package logger

import (
	"fmt"
	"github.com/mgutz/logxi/v1"
)

var logger = log.New("logger")

func InitDefault() {
	logger.SetLevel(log.LevelAll)
}

func formatLogMessage(remoteAddr string, message string, a ...interface{}) string {
	return fmt.Sprintf("[client %s]: %s", remoteAddr, fmt.Sprintf(message, a...))
}

func Warn(remoteAddr string, message string, a ...interface{}) {
	_ = logger.Warn(formatLogMessage(remoteAddr, message, a...))
}

func Info(remoteAddr string, message string, a ...interface{}) {
	logger.Info(formatLogMessage(remoteAddr, message, a...))
}
