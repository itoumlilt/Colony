package observable

import (
	"context"
	"os"
	"os/signal"

	"github.com/confluentinc/confluent-kafka-go/kafka"
	rxgo "github.com/reactivex/rxgo/v2"
)

// KafkaConnect takes the configuration map and topics list and generates a kafka consumer
// used to retrieve messages from the given topic.   This consume can then be handed
// to the KafkaObservable function to generate an observable that may be function mapped,
// filtered and subscribed to.
func KafkaConnect(configMap *kafka.ConfigMap, topics []string) (*kafka.Consumer, error) {
	c, err := kafka.NewConsumer(configMap)
	if err != nil {
		return c, err
	}

	err = c.SubscribeTopics(topics, nil)

	return c, err
}

func systemMonitor(cancelFunc context.CancelFunc) {
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt)
	<-c

	// cancel signal arrived, signal context to shutdown
	cancelFunc()
}

// KafkaObservable takes a kafka consumer and turns it into an rx observable.
func KafkaObservable(consumer *kafka.Consumer) func(context.Context, chan<- rxgo.Item) {
	return func(ctx context.Context, ch chan<- rxgo.Item) {
		_, cancelFunc := context.WithCancel(context.Background())
		go systemMonitor(cancelFunc)
		for {
			msg, err := consumer.ReadMessage(500)
			if err == nil {
				//fmt.Print(".")
				ch <- rxgo.Of(msg)
			} /*else {
				if err.(kafka.Error).Code() == kafka.ErrTimedOut {
					if cancellable.Err() != nil {
						// in this case, the context cancelation is set and we need to close the observable.
						//close(ch)
					}
				}
				ch <- rxgo.Error(err)
			}*/
		}
	}

}
