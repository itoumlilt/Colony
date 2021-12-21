package main

import (
	"flag"
	"fmt"
	colony "github.com/colonyDB/colony-go-client"
	"net"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"
)

func parseHost(hostport string) (host colony.Host, err error) {
	name, port, err := net.SplitHostPort(hostport)
	if err != nil {
		err = fmt.Errorf("error parsing host %v: %v", hostport, err)
		return
	}
	portI, err := strconv.Atoi(port)
	if err != nil {
		err = fmt.Errorf("error parsing host %v: port must be an integer", hostport)
		return
	}
	host = colony.Host{Name: name, Port: portI}
	return
}

func parseHosts(hostports []string) (hosts []colony.Host, err error) {
	hosts = make([]colony.Host, len(hostports))
	for i, v := range hostports {
		host, err := parseHost(v)
		if err != nil {
			return nil, err
		}
		hosts[i] = host
	}
	return hosts, nil

}

var quiet = flag.Bool("quiet", false, "Turn off status messages")

func debug(str string, vals ...interface{}) {
	if !*quiet {
		fmt.Printf(str+"\n", vals...)
	}
}

func retry(retries int, f func() error) (err error) {
	delay := 100 * time.Millisecond
	tries := 0
	for retries <= 0 || tries < retries {
		err = f()
		if err == nil {
			return
		}
		fmt.Printf("ERROR %v\n", err)
		debug("Retrying in %s", delay.String())
		time.Sleep(delay)
		delay *= 2
		tries++
	}
	return
}

func main() {
	createDc := flag.String("createDc", "", "Connects nodes in a DC. The first entry it the colony instance to connect to, the following arguments are Erlang node names to connect. For example:\n"+
		"     colony-connect --createDc server1:8087 colony@server2 colony@server3")
	connectDcs := flag.Bool("connectDcs", false, "Connects several DCs. Takes a list of 'hostname:port' where each entry is a node from a different DC. For example:\n"+
		"     colony-connect --connectDcs server1:8087 server4:8087")
	maxRetries := flag.Int("retries", 0, "Maximum number of retries for connections.")
	flag.Parse()
	tail := flag.Args()

	work := false
	if len(*createDc) > 0 {
		host, err := parseHost(*createDc)
		if err != nil {
			fmt.Println("Error parsing host: ", err)
			os.Exit(1)
		}
		err = retry(*maxRetries, func() error {
			return runCreateDc(host, tail)
		})
		if err != nil {
			os.Exit(1)
		}
		work = true
	}

	if *connectDcs {
		hosts, err := parseHosts(tail)
		if err != nil {
			fmt.Println("Error parsing hosts: ", err)
			os.Exit(1)
		}
		err = retry(*maxRetries, func() error {
			return runConnectDcs(hosts)
		})
		if err != nil {
			os.Exit(1)
		}

		work = true
	}

	if !work {
		flag.Usage()
	}

}

func runCreateDc(host colony.Host, nodes []string) (err error) {
	debug("Connecting to %s", host.Name)
	client, err := colony.NewClient(host)
	if err != nil {
		return fmt.Errorf("Could not connect to colony at %s:%d", host.Name, host.Port)
	}
	defer client.Close()
	debug("Connecting DCs %s", strings.Join(nodes, ", "))
	err = client.CreateDc(nodes)
	if err != nil {
		return fmt.Errorf("Could not create DC: %v", err)

	}
	debug("Done.")
	return
}

func runConnectDcs(connectDcs []colony.Host) (err error) {
	var clients = make([]*colony.Client, len(connectDcs))
	defer func() {
		for _, client := range clients {
			if client != nil {
				client.Close()
			}
		}
	}()

	var descriptors = make([][]byte, len(connectDcs))
	debug("Getting connection descriptors")
	var wg sync.WaitGroup
	var lock = sync.Mutex{}
	errors := make(chan error, len(connectDcs))
	for i, host := range connectDcs {
		wg.Add(1)
		go func(i int, host colony.Host) {
			defer wg.Done()

			client, err := colony.NewClient(host)
			if err != nil {
				debug("%s", err.Error())
				errors <- err
				return
			}
			debug("Connection to %s:%d established. Getting connection descriptor.", host.Name, host.Port)

			descriptor, err := client.GetConnectionDescriptor()

			lock.Lock()
			debug("Got connection descriptor from %s:%d", host.Name, host.Port)
			clients[i] = client
			descriptors[i] = descriptor
			lock.Unlock()
		}(i, host)
	}
	wg.Wait()
	close(errors)
	ok := true
	for err := range errors {
		ok = false
		fmt.Println("Error connecting DCs:", err)
	}
	if !ok {
		return fmt.Errorf("Could not connect DCs")
	}

	errors = make(chan error, len(connectDcs))
	for i := range connectDcs {
		wg.Add(1)
		go func(i int) {
			err := clients[i].ConnectToDCs(descriptors)
			if err != nil {
				errors <- fmt.Errorf("Could not connect %s:%d, %v", connectDcs[i].Name, connectDcs[i].Port, err)
			}
			wg.Done()
			debug("Connection done on %s:%d", connectDcs[i].Name, connectDcs[i].Port)
		}(i)
	}
	wg.Wait()
	close(errors)
	for err := range errors {
		ok = false
		fmt.Println("Error connecting DCs:", err)
	}
	if !ok {
		return fmt.Errorf("Could not connect DCs")
	}
	debug("DCs connected")
	return
}