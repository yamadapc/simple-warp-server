package main

import (
	"fmt"
	"log"
	"net/http"
)

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello World\n")
	})

	fmt.Println("Listening on port 8080")
	e := http.ListenAndServe(":8080", nil)
	log.Fatal(e)
}
