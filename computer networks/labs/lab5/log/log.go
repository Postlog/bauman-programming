package log

import "go.uber.org/zap"

type Logger interface {
	Debug(args ...interface{})
	Info(args ...interface{})
	Warn(args ...interface{})
	Error(args ...interface{})

	Debugf(format string, args ...interface{})
	Infof(format string, args ...interface{})
	Warnf(format string, args ...interface{})
	Errorf(format string, args ...interface{})
}

func New() (Logger, error) {
	return newZapLogger()
}

func newZapLogger() (*zap.SugaredLogger, error) {
	zapLogger, err := zap.NewDevelopment()
	if err != nil {
		return nil, err
	}
	return zapLogger.Sugar(), nil
}
