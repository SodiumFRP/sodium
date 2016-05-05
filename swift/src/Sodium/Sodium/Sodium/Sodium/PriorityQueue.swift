//
//  PriorityQueue.swift
//  Groupies
//
//  Created by Andrew Bradnan on 1/21/16.
//  Copyright © 2016 Microsoft Corp. All rights reserved.
//

import Foundation

class PriorityQueue<T> {
    var contents = [T]()
    var sorted = false
    let comparator: ((T,T)->Bool)
    
    
    init(comparator: (T,T)->Bool) {
        self.comparator = comparator
    }
    
    func min(before: (T,T) throws -> Bool) throws -> T? {
        return try contents.minElement(before)
    }
    
    func sort(cmp: ((T,T) -> Bool)? = nil) {
        contents.sortInPlace(cmp != nil ? cmp! : comparator)
        sorted = true
    }
    
    func find(predicate: T -> Bool) -> T? {
        if let idx = contents.indexOf(predicate) {
            return contents[idx]
        }
        return nil
    }
    
    func push(o: T) {
        contents.append(o)
        sorted = false
    }
    
    func peek(index: Int? = nil) -> T {
        if !sorted { sort() }
        
        let idx = index ?? contents.count - 1
        return contents[idx]
    }
    
    func pop() -> T? {
        if !sorted { sort() }
        
        return contents.popLast()
    }
    
    var first: T? {
        get {
            if !sorted { sort() }
            
            return contents.first
        }
        set(value) {
            if contents.isEmpty {
                contents.append(value!)
            }
            else {
                contents[0] = value!
            }
            sorted = false
        }
    }
    
    var last: T? {
        get {
            if !sorted { sort() }
            
            return contents.last
        }
        set(value) {
            let idx = contents.count - 1
            if idx < 0 {
                contents.append(value!)
            }
            else {
                contents[idx] = value!
            }
            sorted = false
        }
    }
    
    func removeAll() { contents.removeAll() }
    
    var count: Int { return contents.count }
    var isEmpty: Bool { return contents.isEmpty }

    func map<U>(f: (T) -> U) -> [U] {
        return contents.map(f)
    }
}
