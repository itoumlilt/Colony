"use strict";

import '../proto/colony_proto'
import ByteBuffer = require("bytebuffer")
import { colonyConnection } from "./colonyConnection"
import { MessageCodes } from "./messageCodes"
import * as Long from "long";
import msgpack = require("msgpack-lite")

/** Connects to colony on the given port and hostname
 * @param port the port number of colony's protocol buffer port (for example 8087)
 * @param host the host running colony (for example "localhost")
 */
export function connect(port: number, host: string): Connection {
	return new ConnectionImpl(new colonyConnection(port, host))

}

/** Creates a BoundObject, wich colony uses as key for data */
function key(key: string, type: colonyPB.CRDT_type, bucket: string): colonyPB.ApbBoundObject {
	return {
		key: ByteBuffer.fromUTF8(key),
		type: type,
		bucket: ByteBuffer.fromUTF8(bucket)
	}
}

/** takes a message with an encode method and converts it into an ArrayBuffer */
function encode(message: { encode(): ByteBuffer } | any): ArrayBuffer {
	return message.encode().toBuffer()
}

function _debugPrint(obj: any): string {
	return JSON.stringify(obj, (key, val) => {
		if (val instanceof ByteBuffer) {
			return val.toUTF8();
		} else if (val instanceof Long) {
			return val.toNumber();
		}
		return val;
	});
}


/**
 * A CRDT factory is used to create references to stored objects.
 * These references are linked to the factory from which they were created.
 * 
 * There are three kind of factories: the [[Connection]], [[Transaction]]s and [[CrdtMap]]s.
 * 
 */
export interface CrdtFactory {

	/** returns a reference to a counter object */
	counter(key: string): CrdtCounter;

	/** returns a reference to a fat_counter object */
	fatCounter(key: string): CrdtCounter;

	/** returns a reference to a last-writer-wins register */
	register<T>(key: string): CrdtRegister<T>;

	/** returns a reference to a multi-value register */
	multiValueRegister<T>(key: string): CrdtMultiValueRegister<T>;

	/** returns a reference to an enable-wins flag object */
	flag_ew(key: string): CrdtFlag;

	/** returns a reference to an disable-wins flag object */
	flag_dw(key: string): CrdtFlag;

	/** returns a reference to an add-wins set object */
	set<T>(key: string): CrdtSet<T>;

	/** returns a reference to a remove-wins set object */
	set_removeWins<T>(key: string): CrdtSet<T>;

	/** returns a reference to a remove-resets map */
	rrmap(key: string): CrdtMap;

	/** returns a reference to a grow-only map */
	gmap(key: string): CrdtMap;
}


abstract class CrdtFactoryImpl implements CrdtFactory {


	abstract getBucket(): string;

	public abstract readBatch(objects: colonyObject<any>[]): Promise<any[]>;

	public abstract jsToBinary(obj: any): ByteBuffer;

	public abstract binaryToJs(byteBuffer: ByteBuffer): any;

	/** returns a reference to a counter object */
	public counter(key: string): CrdtCounter {
		return new CrdtCounterImpl(this, key, this.getBucket(), colonyPB.CRDT_type.COUNTER);
	}

	/** returns a reference to a fat-counter object */
	public fatCounter(key: string): CrdtCounter {
		return new CrdtCounterImpl(this, key, this.getBucket(), colonyPB.CRDT_type.FATCOUNTER);
	}

	/** returns a reference to a last-writer-wins register */
	public register<T>(key: string): CrdtRegister<T> {
		return new CrdtRegisterImpl<T>(this, key, this.getBucket(), colonyPB.CRDT_type.LWWREG);
	}

	/** returns a reference to a multi-value register */
	public multiValueRegister<T>(key: string): CrdtMultiValueRegister<T> {
		return new CrdtMultiValueRegisterImpl<T>(this, key, this.getBucket(), colonyPB.CRDT_type.MVREG);
	}

	/** returns a reference to an enable-wins flag object */
	public flag_ew(key: string): CrdtFlag {
		return new CrdtFlagImpl(this, key, this.getBucket(), colonyPB.CRDT_type.FLAG_EW);
	}

	/** returns a reference to an disable-wins flag object */
	public flag_dw(key: string): CrdtFlag {
		return new CrdtFlagImpl(this, key, this.getBucket(), colonyPB.CRDT_type.FLAG_EW);
	}


	/** returns a reference to a add-wins set object */
	public set<T>(key: string): CrdtSet<T> {
		return new CrdtSetImpl<T>(this, key, this.getBucket(), colonyPB.CRDT_type.ORSET);
	}

	/** returns a reference to a remove-wins set object */
	public set_removeWins<T>(key: string): CrdtSet<T> {
		return new CrdtSetImpl<T>(this, key, this.getBucket(), colonyPB.CRDT_type.RWSET);
	}

	/** returns a reference to a remove-resets map */
	public rrmap(key: string): CrdtMap {
		return new CrdtMapImpl(this, key, this.getBucket(), colonyPB.CRDT_type.RRMAP);
	}

	/** returns a reference to a grow-only map */
	public gmap(key: string): CrdtMap {
		return new CrdtMapImpl(this, key, this.getBucket(), colonyPB.CRDT_type.GMAP);
	}


	abstract childUpdate(key: colonyPB.ApbBoundObject, operation: colonyPB.ApbUpdateOperation): colonyPB.ApbUpdateOp;

	readResponseToValue(type: colonyPB.CRDT_type, response: colonyPB.ApbReadObjectResp): any {
		let obj: colonyObject<any>;
		switch (type) {
			case colonyPB.CRDT_type.COUNTER:
				obj = this.counter("");
				break;
			case colonyPB.CRDT_type.ORSET:
				obj = this.set("");
				break;
			case colonyPB.CRDT_type.LWWREG:
				obj = this.register("");
				break;
			case colonyPB.CRDT_type.MVREG:
				obj = this.multiValueRegister("");
				break;
			case colonyPB.CRDT_type.GMAP:
				obj = this.gmap("");
				break;
			case colonyPB.CRDT_type.RWSET:
				obj = this.set_removeWins("");
				break;
			case colonyPB.CRDT_type.RRMAP:
				obj = this.rrmap("");
				break;
			case colonyPB.CRDT_type.FATCOUNTER:
				obj = this.fatCounter("");
				break;
			case colonyPB.CRDT_type.FLAG_EW:
				obj = this.flag_ew("");
				break;
			case colonyPB.CRDT_type.FLAG_DW:
				obj = this.flag_dw("");
				break;
			default:
				throw new Error(`unhandled type: ${type}`);
		}
		return (obj as colonyObjectImpl<any>).interpretReadResponse(response)
	}
}

/**
 * An `colonySession` is an interface to colony, which can be used to read and update values.
 * 
 * There are two possible sessions:  
 * 
 *  - The [[Connection]] for reads and updates which are not part of interactive transactions.
 *  - [[Transaction]] for performing reads and updates within an interactive transaction.
 */
export interface colonySession extends CrdtFactory {
	/**
	 * Takes an array of objects and reads the value of all objects in one batch operation.
	 * Returns a promise to an array of values in the same order.
	 *
	 * Hint: To read a single object, use the read method on that object.
	 */
	readBatch(objects: colonyObject<any>[]): Promise<any[]>;

	/**
	 * Reads several objects at once.
	 * The objects are stored in an object.
	 * Returns a new object with the read values stored under the same field in the object.
	 *
	 * 		let objA = connection.counter("batch-object-read counter a")
	 *		let objB = connection.register<string>("batch-object-read register b")
     *
	 *		let vals = await connection.readObjectsBatch({
	 *			a: objA,
	 *			b: objB
	 *		});
	 *		// could return: {a: 1, b: "hi"}
	 *
	 * Hint: To read a single object, use the read method on that object.
	 */
	readObjectsBatch<T>(objects: {[K in keyof T]: colonyObject<T[K]>}): Promise<{[K in keyof T]: T[K]}>;


	/**
	 * Sends a single update operation or an array of update operations to colony.
	 * If an array of updates is given, all updates in the array are executed atomically.
	 */
	update(updates: colonyPB.ApbUpdateOp[] | colonyPB.ApbUpdateOp): Promise<any>;
}


/** A connection to colonyDB with methods for reading, updating and starting transactions.
 * Use the [[connect]] function to obtain a `Connection`.
 * 
 * The Connection can then be used as a [[CrdtFactory]] to create references to database objects.
 * 
 * The [[readBatch]] and [[update]] methods can be used to perform reads and updates.
 * 
 * Example:
 * 
 * ```
 * let colony = colonyClient.connect(8087, "localhost")
 * // create a reference to a set object:
 * let userSet = colony.set("users")
 * // read the value of the set
 * let val = await userSet.read()
 * // update the set:
 * await colony.update(userSet.add("Hans"))
 * 
 * ```
 * 
 * The bucket can be configured via the property `defaultBucket`, it defaults to "default-bucket".
 * 
 * Javascript objects stored in sets and registers are encoded using MessagePack (http://msgpack.org) by default.
 * You can override the [[jsToBinary]] and [[binaryToJs]] methods to customize this behavior.
 * 
 */
export interface Connection extends colonySession {

	/**
	 * The minimum snapshot version to use for new transactions.
	 * This will be used when starting a new transaction in order to guarantee
	 * session guarantees like monotonic reads and read-your-writes */
	minSnapshotTime: ByteBuffer | undefined;


	/**
	 * Option, which determines if snapshots should be monotonic.
	 * If set to `true`, this will update minSnapshotTime whenever 
	 * lastCommitTimestamp is updated
	 */
	monotonicSnapshots: boolean;

	/**
	 * the default bucket used for newly created keys
	 */
	defaultBucket: string;

	/**
	 * The DataFormat to use for decoding and encoding binary values.
	 * The default is [[MsgpackDataFormat]].
	 */
	dataFormat: DataFormat;

	/** Method to encode objects before they are written to the database */
	jsToBinary(obj: any): ByteBuffer;

	/** Inverse of jsToBinary */
	binaryToJs(byteBuffer: ByteBuffer): any;

	/** Sets the timout for requests */
	setTimeout(ms: number): void;

	/** Starts a new transaction */
	startTransaction(): Promise<Transaction>;

	/**
	 * returns the timestamp for the last commited transaction
	 */
	getLastCommitTimestamp(): ByteBuffer | undefined;

	/**
	 * Closes the connection to colony
	 */
	close(): void;

}

/**
 * A DataFormat tells colony how JavaScript values should be stored in
 * the database (in Sets, Registers, Maps).
 * A DataFormat has to implement two functions jsToBinary and binaryToJs to convert between binary data and JavaScript values.
 *
 * The default implementation is [[MsgpackDataFormat]].
 */
export interface DataFormat {
	/** Method to encode objects before they are written to the database */
	jsToBinary(obj: any): ByteBuffer;

	/** Inverse of jsToBinary */
	binaryToJs(byteBuffer: ByteBuffer): any;
}

/**
 * A DataFormat, which encodes/decodes data with MessagePack (see http://msgpack.org)
 */
export class MsgpackDataFormat implements DataFormat {
	/** Method to encode objects before they are written to the database */
	public jsToBinary(obj: any): ByteBuffer {
		// TODO there must be a better way to do this
		let buffer: Buffer = msgpack.encode(obj);
		let res = new ByteBuffer();
		res.append(buffer);
		res.flip();
		return res;
	}

	/** Inverse of jsToBinary */
	public binaryToJs(byteBuffer: ByteBuffer): any {
		if (byteBuffer.remaining() <= 0) {
			return null;
		}
		let buffer = new Buffer(byteBuffer.toArrayBuffer());
		let decoded = msgpack.decode(buffer);
		return decoded
	}
}

class ConnectionImpl extends CrdtFactoryImpl implements Connection {
	readonly connection: colonyConnection;
	/**
	 * stores the last commit time.
	 */
	private lastCommitTimestamp: ByteBuffer | undefined = undefined;

	/**
	 * The minimum snapshot version to use for new transactions.
	 * This will be used when starting a new transaction in order to guarantee
	 * session guarantees like monotonic reads and read-your-writes */
	public minSnapshotTime: ByteBuffer | undefined = undefined;


	/**
	 * Option, which determines if snapshots should be monotonic.
	 * If set to `true`, this will update minSnapshotTime whenever 
	 * lastCommitTimestamp is updated
	 */
	public monotonicSnapshots: boolean = false;

	/**
	 * the default bucket used for newly created keys
	 */
	public defaultBucket = "default-bucket";

	/**
	 * The DataFormat to use for decoding and encoding binary values.
	 * The default is [[MsgpackDataFormat]].
	 */
	public dataFormat: DataFormat = new MsgpackDataFormat();


	constructor(conn: colonyConnection) {
		super();
		this.connection = conn;
	}

	public getBucket(): string {
		return this.defaultBucket;
	}

	childUpdate(key: colonyPB.ApbBoundObject, operation: colonyPB.ApbUpdateOperation): colonyPB.ApbUpdateOp {
		var op = {
			boundobject: key,
			operation: operation
		};
		return op;
	}


	/** Method to encode objects before they are written to the database */
	public jsToBinary(obj: any): ByteBuffer {
		return this.dataFormat.jsToBinary(obj);
	}

	/** Inverse of jsToBinary */
	public binaryToJs(byteBuffer: ByteBuffer): any {
		return this.dataFormat.binaryToJs(byteBuffer);
	}

	/** Sets the timout for requests */
	public setTimeout(ms: number) {
		this.connection.requestTimeoutMs = ms;
	}

	/** Starts a new transaction */
	public async startTransaction(): Promise<Transaction> {
		let apbStartTransaction = MessageCodes.colonyPb.ApbStartTransaction;
		let message: colonyPB.ApbStartTransactionMessage = new apbStartTransaction(this.startTransactionPb());
		let resp: colonyPB.ApbStartTransactionResp = await this.connection.sendRequest(MessageCodes.apbStartTransaction, encode(message));
		if (resp.success) {
			return new TransactionImpl(this, resp.transaction_descriptor!);
		}
		return Promise.reject<any>(resp.errorcode);
	}


	/**
	 * returns the timestamp for the last commited transaction
	 */
	public getLastCommitTimestamp(): ByteBuffer | undefined {
		return this.lastCommitTimestamp;
	}


	private setLastCommitTimestamp(lastCommitTimestamp: ByteBuffer | undefined) {
		this.lastCommitTimestamp = lastCommitTimestamp;
		if (this.monotonicSnapshots) {
			this.minSnapshotTime = lastCommitTimestamp;
		}
	}

	/**
	 * creates a startTransaction message with the last timestamp
	 * and default transaction properties
	 */
	private startTransactionPb(): colonyPB.ApbStartTransaction {
		return {
			timestamp: this.minSnapshotTime,
			properties: {}
		};
	}

	/** 
	 * Reads several objects at once.
	 * To read a single object, use the read method on that object.
	 */
	public async readBatch(objects: colonyObject<any>[]): Promise<any[]> {
		let objects2 = objects as colonyObjectImpl<any>[]
		let messageType = MessageCodes.colonyPb.ApbStaticReadObjects;
		let message: colonyPB.ApbStaticReadObjectsMessage = new messageType({
			transaction: this.startTransactionPb(),
			objects: objects2.map(o => o.key)
		});
		let resp: colonyPB.ApbStaticReadObjectsResp = await this.connection.sendRequest(MessageCodes.apbStaticReadObjects, encode(message));
		let cr = this.completeTransaction(resp.committime!);
		let readResp = resp.objects!;
		if (readResp.success) {
			let resVals: any[] = [];

			for (let i in objects2) {
				var obj = objects2[i];
				resVals.push(obj.interpretReadResponse(readResp.objects![i]))
			}

			this.lastCommitTimestamp = cr.commitTime;
			return Promise.resolve(resVals);
		} else {
			return Promise.reject<any[]>(readResp.errorcode)
		}
	}

	/**
	 * Reads several objects at once.
	 * The objects are stored in an object.
	 * Returns a new object with the read values stored under the same field in the object.
	 *
	 * 		let objA = connection.counter("batch-object-read counter a")
	 *		let objB = connection.register<string>("batch-object-read register b")
     *
	 *		let vals = await connection.readObjectsBatch({
	 *			a: objA,
	 *			b: objB
	 *		});
	 *		// could return: {a: 1, b: "hi"}
	 *
	 * Hint: To read a single object, use the read method on that object.
	 */
	public async readObjectsBatch<T>(objects: {[K in keyof T]: colonyObject<T[K]>}): Promise<{[K in keyof T]: T[K]}> {
		let messageType = MessageCodes.colonyPb.ApbStaticReadObjects;
		let keys: (keyof T)[] = Object.keys(objects) as any[];
		let objectArray = keys.map(key => objects[key]);
		let results = await this.readBatch(objectArray);
		let resObj: any = {};
		for (let i in keys) {
			let key = keys[i];
			let result = results[i];
			resObj[key] = result;
		}
		return resObj;
	}

	/**
	 * Sends a single update operation or an array of update operations to colony.
	 */
	public async update(updates: colonyPB.ApbUpdateOp[] | colonyPB.ApbUpdateOp): Promise<CommitResponse> {
		let messageType = MessageCodes.colonyPb.ApbStaticUpdateObjects;
		let updatesAr: colonyPB.ApbUpdateOp[] = (updates instanceof Array) ? updates : [updates];
		let message: colonyPB.ApbStaticUpdateObjectsMessage = new messageType({
			transaction: this.startTransactionPb(),
			updates: updatesAr
		});
		let resp = await this.connection.sendRequest(MessageCodes.apbStaticUpdateObjects, encode(message));
		return this.completeTransaction(resp)
	}

	completeTransaction(resp: colonyPB.ApbCommitResp): CommitResponse {
		if (resp.commit_time) {
			this.lastCommitTimestamp = resp.commit_time;
		}
		if (resp.success) {
			return {
				commitTime: resp.commit_time!
			};
		}
		throw new Error(`Failed to commit transaction (Error code: ${resp.errorcode})`);
	}

	/**
	 * Closes the connection to colony
	 */
	public close() {
		this.connection.close();
	}


}



interface CommitResponse {
	commitTime: ByteBuffer
}

/**
 * A transaction can be used similar to a [[Connection]] to get references to database and objects
 * and to perform reads and updates.
 * 
 * Example:
 * ```
 *     let tx = await colony.startTransaction()
 *     // create object reference bound to the transaction:
 *     let reg = tx.multiValueRegister<number>("some-key");
 *     
 *     // read the register in the transaction:
 *     let vals = await reg.read();
 *     
 *     // update the register based on current values 
 *     let newval = f(vals) 
 *     await tx.update(
 *         reg.set(newval)
 *     )
 *     await tx.commit()
 * ```
 * 
 */
export interface Transaction extends colonySession {

	/**
	 * Commits the transaction.
	 */
	commit(): Promise<any>;

	/**
	 * Aborts the transaction.
	 */
	abort(): Promise<void>;
}

class TransactionImpl extends CrdtFactoryImpl implements Transaction {
	private connection: ConnectionImpl;
	private colonyConnection: colonyConnection;
	private txId: ByteBuffer;
	constructor(conn: ConnectionImpl, txId: ByteBuffer) {
		super();
		this.connection = conn;
		this.colonyConnection = conn.connection;
		this.txId = txId;
	}

	public getBucket() {
		return this.connection.getBucket();
	}

	public jsToBinary(obj: any): ByteBuffer {
		return this.connection.jsToBinary(obj);
	}

	public binaryToJs(byteBuffer: ByteBuffer): any {
		return this.connection.binaryToJs(byteBuffer);
	}

	childUpdate(key: colonyPB.ApbBoundObject, operation: colonyPB.ApbUpdateOperation): colonyPB.ApbUpdateOp {
		return this.connection.childUpdate(key, operation);
	}

	/** 
	 * Reads several objects at once.
	 */
	public async readBatch(objects: colonyObject<any>[]): Promise<any[]> {
		let objects2 = objects as colonyObjectImpl<any>[];
		let apb = MessageCodes.colonyPb.ApbReadObjects;
		let message = new apb({
			boundobjects: objects2.map(o => o.key),
			transaction_descriptor: this.txId
		});
		let resp: colonyPB.ApbReadObjectsResp = await this.colonyConnection.sendRequest(MessageCodes.apbReadObjects, encode(message));
		if (resp.success) {
			let resVals: any[] = [];
			for (let i in objects2) {
				var obj = objects2[i];
				let objVal = obj.interpretReadResponse(resp.objects![i]);
				resVals.push(objVal)
			}
			return resVals;
		}
		return Promise.reject<any[]>(resp.errorcode)
	}


	/**
	 * Reads several objects at once.
	 */
	public async readObjectsBatch<T>(objects: {[K in keyof T]: colonyObject<T[K]>}): Promise<{[K in keyof T]: T[K]}> {
		let messageType = MessageCodes.colonyPb.ApbStaticReadObjects;
		let keys: (keyof T)[] = Object.keys(objects) as any[];
		let objectArray = keys.map(key => objects[key]);
		let results = await this.readBatch(objectArray);
		let resObj: any = {};
		for (let i in keys) {
			let key = keys[i];
			let result = results[i];
			resObj[key] = result;
		}
		return resObj;
	}

	/**
	 * Sends a single update operation or an array of update operations to colony.
	 */
	public async update(updates: colonyPB.ApbUpdateOp[] | colonyPB.ApbUpdateOp): Promise<void> {
		let messageType = MessageCodes.colonyPb.ApbUpdateObjects;
		let updatesAr: colonyPB.ApbUpdateOp[] = (updates instanceof Array) ? updates : [updates];
		let message = new messageType({
			transaction_descriptor: this.txId,
			updates: updatesAr
		});
		await this.colonyConnection.sendRequest(MessageCodes.apbUpdateObjects, encode(message));
	}

	public async commit(): Promise<CommitResponse> {
		let apbCommitTransaction = MessageCodes.colonyPb.ApbCommitTransaction;
		let message: colonyPB.ApbCommitTransactionMessage = new apbCommitTransaction({
			transaction_descriptor: this.txId
		});
		let resp = await this.colonyConnection.sendRequest(MessageCodes.apbCommitTransaction, encode(message));
		return this.connection.completeTransaction(resp)
	}

	public async abort(): Promise<void> {
		let apbAbortTransaction = MessageCodes.colonyPb.ApbAbortTransaction;
		let message: colonyPB.ApbCommitTransactionMessage = new apbAbortTransaction({
			transaction_descriptor: this.txId
		});
		let resp = await this.colonyConnection.sendRequest(MessageCodes.apbAbortTransaction, encode(message));
	}

	public toString(): string {
		return `Transaction ${this.txId.toBinary()}`
	}
}


/**
 * An colonyObject is a reference to an object in the database and is bound to
 * the [[CrdtFactory]] which created the reference.
 * 
 * For example, when a reference is created from a [[Transaction]] object, 
 * all reads on the object will be performed in the context of the transaction.
 * 
 * @param T the type returned when reading the object
 */
export interface colonyObject<T> {
	/** the parent factory */
	readonly parent: CrdtFactory;

	/** 
	 * reads the current value of the object  
	 **/
	read(): Promise<T>;

}

abstract class colonyObjectImpl<T> implements colonyObject<T> {
	readonly parent: CrdtFactoryImpl;
	readonly key: colonyPB.ApbBoundObject;

	constructor(conn: CrdtFactoryImpl, key: string, bucket: string, type: colonyPB.CRDT_type) {
		this.parent = conn;
		this.key = {
			key: ByteBuffer.fromUTF8(key),
			bucket: ByteBuffer.fromUTF8(bucket),
			type: type
		}
	}

	makeUpdate(operation: colonyPB.ApbUpdateOperation): colonyPB.ApbUpdateOp {
		return this.parent.childUpdate(this.key, operation);
	}

	abstract interpretReadResponse(readResponse: colonyPB.ApbReadObjectResp): any;

	public async read(): Promise<T> {
		let r = await this.parent.readBatch([this])
		return r[0]
	}
}


/**
 * A counter is a object that stores a single integer and can be incremented or decremented.
 * 
 * Example:
 * 
 * ```
 * let counter = connection.counter("myCounter")
 * await connection.update(
 * 	counter.increment(3)
 * );
 * let val = await counter.read();
 * ```
 * 
 */
export interface CrdtCounter extends colonyObject<number> {
	/** Creates an operation to increment the counter.
	 * Negative numbers will decrement the value. 
	 * Use [[Connection.update]] to send the update to the database. */
	increment(amount: number | Long): colonyPB.ApbUpdateOp;

	/**
	 * Reads the current value of the counter
	 */
	read(): Promise<number>;

}

class CrdtCounterImpl extends colonyObjectImpl<number> implements CrdtCounter {

	interpretReadResponse(readResponse: colonyPB.ApbReadObjectResp): number {
		return readResponse.counter!.value!;
	}

	/** Creates an operation to increment the counter.
	 * Negative numbers will decrement the value. 
	 * Use [[[[Connection.update]]]] to send the update to the database. */
	public increment(amount: number | Long): colonyPB.ApbUpdateOp {
		let amountL = (amount instanceof Long) ? amount : Long.fromNumber(amount);
		return this.makeUpdate({
			counterop: {
				inc: amountL
			}
		})
	}

}

/**
 * A flag stores a boolean value, that can be changed
 *
 * ```
 * let flag = connection.flag_ew("myflag")
 * await connection.update(
 * 	flag.set(true)
 * )
 * let val = await flag.read();
 * ```
*/
export interface CrdtFlag extends colonyObject<boolean> {
	/** Creates an operation to set the flag to the given value.
	 * Use [[Connection.update]] to send the update to the database. */
	set(value: boolean): colonyPB.ApbUpdateOp;
}


class CrdtFlagImpl extends colonyObjectImpl<boolean> implements CrdtFlag {
	interpretReadResponse(readResponse: colonyPB.ApbReadObjectResp): boolean {
		return readResponse.flag!.value!;
	}

	/** Creates an operation to set the flag to the given value.
	 * Use [[Connection.update]] to send the update to the database. */
	public set(value: boolean): colonyPB.ApbUpdateOp {
		return this.makeUpdate({
			flagop: {
				value: value
			}
		})
	}
}

/**
 * A register stores a single value.
 * It provides a [[set]] method to change the value.
 * 
 * Example:
 * 
 * ```
 * let reg = connection.register<string[]>("mylwwreg")
 * await connection.update(
 * 	reg.set(["a", "b"])
 * )
 * let val = await reg.read();
 * ```
 * 
 * @param T the type of the value stored in the register
 */
export interface CrdtRegister<T> extends colonyObject<T> {
	/** Creates an operation, which sets the register to the provided value.
	 * Use [[Connection.update]] to send the update to the database. */
	set(value: T): colonyPB.ApbUpdateOp;
}

class CrdtRegisterImpl<T> extends colonyObjectImpl<T> implements CrdtRegister<T> {

	interpretReadResponse(readResponse: colonyPB.ApbReadObjectResp): T {
		let bin = readResponse.reg!.value!;
		return this.parent.binaryToJs(bin);
	}

	/** Creates an operation, which sets the register to the provided value.
	 * 
	 * Use [[Connection.update]] to send the update to the database. */
	public set(value: T): colonyPB.ApbUpdateOp {
		let bin = this.parent.jsToBinary(value);
		return this.makeUpdate({
			regop: {
				value: bin
			}
		})
	}

}


/**
 * This register can be [[set]] to a single value, but reading the register returns a list of 
 * all concurrently written values.
 * 
 * Example:
 * 
 * ```
 * let reg = connection.multiValueRegister<number>("mymvreg")
 * await connection.update(
 * 	reg.set(15)
 * )
 * let val = await reg.read();
 * // val is now [15]
 * ```
 * 
 * @param T the type of the value stored in the register
 */
export interface CrdtMultiValueRegister<T> extends colonyObject<T[]> {
	/** Creates an operation, which sets the register to the provided value.
	 * 
	 * Use [[Connection.update]] to send the update to the database. */
	set(value: T): colonyPB.ApbUpdateOp;
}

class CrdtMultiValueRegisterImpl<T> extends colonyObjectImpl<T[]> implements CrdtMultiValueRegister<T> {


	interpretReadResponse(readResponse: colonyPB.ApbReadObjectResp): T[] {
		let bins = readResponse.mvreg!.values!;
		let res: any = bins.map(bin => this.parent.binaryToJs(bin));
		return res;
	}

	/** Creates an operation, which sets the register to the provided value.
	 * Negative numbers will decrement the value. 
	 * Use [[Connection.update]] to send the update to the database. */
	public set(value: T): colonyPB.ApbUpdateOp {
		let bin = this.parent.jsToBinary(value);
		return this.makeUpdate({
			regop: {
				value: bin
			}
		})
	}

}

/**
 * A set of elements.
 * Elements can be added and removed.
 * 
 * Example:  
 * ```
 * let set = setType.create<string>("my-set")
 * await connection.update(
 * 	set.addAll(["a", "b", "c", "d", "e"])
 * )
 * await connection.update([
 * 	set.remove("a"),
 * 	set.removeAll(["b", "c"])
 * ])
 * let val = await set.read();
 * // val is now ["d", "e"]
 * ```
 * 
 * @param T the type of the elements stored in the set
 */
export interface CrdtSet<T> extends colonyObject<T[]> {
	/** 
	 * Creates an operation, which adds an element to the set. 
	 * Use [[Connection.update]] to send the update to the database. */
	add(elem: T): colonyPB.ApbUpdateOp;

	/** 
	 * Creates an operation, which adds several elements to the set. 
	 * Use [[Connection.update]] to send the update to the database. */
	addAll(elems: T[]): colonyPB.ApbUpdateOp;

	/** 
	 * Creates an operation, which removes an element from the set. 
	 * Use [[Connection.update]] to send the update to the database. */
	remove(elem: T): colonyPB.ApbUpdateOp;

	/** 
	 * Creates an operation, which removes several elements from the set. 
	 * Use [[Connection.update]] to send the update to the database. */
	removeAll(elems: T[]): colonyPB.ApbUpdateOp;

}

class CrdtSetImpl<T> extends colonyObjectImpl<T[]> implements CrdtSet<T> {
	interpretReadResponse(readResponse: colonyPB.ApbReadObjectResp): T[] {
		let vals = readResponse.set!.value!;
		return vals.map(bin => {
			return this.parent.binaryToJs(bin)
		});
	}

	/** 
	 * Creates an operation, which adds an element to the set. 
	 * Use [[Connection.update]] to send the update to the database. */
	public add(elem: T): colonyPB.ApbUpdateOp {
		return this.makeUpdate({
			setop: {
				optype: colonyPB.ApbSetUpdate.SetOpType.ADD,
				adds: [this.parent.jsToBinary(elem)],
				rems: []
			}
		})
	}

	/** 
	 * Creates an operation, which adds several elements to the set. 
	 * Use [[Connection.update]] to send the update to the database. */
	public addAll(elems: T[]): colonyPB.ApbUpdateOp {
		return this.makeUpdate({
			setop: {
				optype: colonyPB.ApbSetUpdate.SetOpType.ADD,
				adds: elems.map(elem => this.parent.jsToBinary(elem)),
				rems: []
			}
		})
	}

	/** 
	 * Creates an operation, which removes an element from the set. 
	 * Use [[Connection.update]] to send the update to the database. */
	public remove(elem: T): colonyPB.ApbUpdateOp {
		return this.makeUpdate({
			setop: {
				optype: colonyPB.ApbSetUpdate.SetOpType.REMOVE,
				adds: [],
				rems: [this.parent.jsToBinary(elem)]
			}
		})
	}

	/** 
	 * Creates an operation, which removes several elements from the set. 
	 * Use [[Connection.update]] to send the update to the database. */
	public removeAll(elems: T[]): colonyPB.ApbUpdateOp {
		return this.makeUpdate({
			setop: {
				optype: colonyPB.ApbSetUpdate.SetOpType.REMOVE,
				adds: [],
				rems: elems.map(elem => this.parent.jsToBinary(elem))
			}
		})
	}
}

/**
 * An object representing the value of a [[CrdtMap]].
 */
export interface CrdtMapValue {
	/**
	 * reads the entry with the given key and type
	 */
	get(key: string, type: colonyPB.CRDT_type): any;

	/** reads the counter value with the given key */
	counterValue(key: string): number | undefined;
	/** reads the set value with the given key */
	setValue(key: string): any[] | undefined;
	/** reads the register value with the given key */
	registerValue(key: string): any;
	/** reads the multi-value-register value with the given key */
	mvRegisterValue(key: string): any[] | undefined;
	/** reads the gmap value with the given key */
	gmapValue(key: string): CrdtMapValue | undefined;
	/** reads the remove-wins-set value with the given key */
	rwsetValue(key: string): any[] | undefined;
	/** reads the flag_ew-value with the given key */
	flag_ewValue(key: string): boolean;
	/** reads the flag_dw-value with the given key */
	flag_dwValue(key: string): boolean;

	/** 
	 * Converts this CRDTMapValue into a JavaScript object.
	 * The value of each embedded CRDT is stored under it's key.
	 * 
	 * Warning: If there are two entries with the same keys but different types, then only one of them survives.
	 * */
	toJsObject(): any;
}

class CrdtMapValueImpl implements CrdtMapValue {

	constructor(private factory: CrdtFactoryImpl, private entries: colonyPB.ApbMapEntry[]) {
	}

	public get(key: string, type: colonyPB.CRDT_type): any {
		for (let entry of this.entries) {
			let entryKey = entry.key!;
			if (entryKey.type === type && entryKey.key!.toUTF8() === key) {
				return this.factory.readResponseToValue(type, entry.value!);
			}
		}
		return undefined;
	}

	public counterValue(key: string): number | undefined {
		return this.get(key, colonyPB.CRDT_type.COUNTER)
	}
	public setValue(key: string): any[] | undefined {
		return this.get(key, colonyPB.CRDT_type.ORSET)
	}
	public registerValue(key: string): any {
		return this.get(key, colonyPB.CRDT_type.LWWREG)
	}
	public mvRegisterValue(key: string): any[] | undefined {
		return this.get(key, colonyPB.CRDT_type.MVREG)
	}
	public gmapValue(key: string): CrdtMapValue | undefined {
		return this.get(key, colonyPB.CRDT_type.GMAP)
	}
	public rwsetValue(key: string): any[] | undefined {
		return this.get(key, colonyPB.CRDT_type.RWSET)
	}
	public flag_ewValue(key: string): boolean {
		return this.get(key, colonyPB.CRDT_type.FLAG_EW)
	}
	public flag_dwValue(key: string): boolean {
		return this.get(key, colonyPB.CRDT_type.FLAG_DW)
	}

	public toJsObject(): any {
		let res: any = {};
		for (let entry of this.entries) {
			let type = entry.key!.type!;
			let value = this.factory.readResponseToValue(type, entry.value!);
			if (value instanceof CrdtMapValueImpl) {
				value = value.toJsObject();
			}
			res[entry.key!.key!.toUTF8()] = value;
		}
		return res;
	}
}

/**
 * A map with embedded CRDTs. 
 * Each map implements the [[CrdtFactory]] interface, so it can be used like a connection to create references to embedded objects.
 * The [[remove]] and [[removeAll]] methods can be used to remove entries from the map.  
 * 
 * Example:
 * 
 * ```
 * let map = connection.map("my-map2");
 * await connection.update([
 * 	map.register("a").set("x"),
 * 	map.register("b").set("x"),
 * 	map.register("c").set("x"),
 * 	map.register("d").set("x"),
 * 	map.set("e").addAll([1, 2, 3, 4]),
 * 	map.counter("f").increment(5)
 * ])
 * await connection.update([
 * 	map.remove(map.register("a")),
 * 	map.removeAll([map.register("b"), map.register("c")])
 * ])
 * let val = await map.read();
 * // convert CrdtMapValue to JavaScript object:
 * let obj = val.toJsObject();
 * // obj is now { d: "x", e: [1, 2, 3, 4], f: 5 }
 * ```
 */
export interface CrdtMap extends colonyObject<CrdtMapValue>, CrdtFactory {

	/**
	 * Creates an operation to remove an entry from the map.
	 * Use [[Connection.update]] to send the update to the database.
	 */
	remove(object: colonyObject<any>): colonyPB.ApbUpdateOp

	/**
	 * Creates an operation to remove several entries from the map.
	 * Use [[Connection.update]] to send the update to the database.
	 */
	removeAll(objects: colonyObject<any>[]): colonyPB.ApbUpdateOp
}


class CrdtMapImpl extends CrdtFactoryImpl implements CrdtMap {
	readonly parent: CrdtFactoryImpl;
	readonly key: colonyPB.ApbBoundObject;

	constructor(conn: CrdtFactoryImpl, key: string, bucket: string, type: colonyPB.CRDT_type) {
		super();
		this.parent = conn;
		this.key = {
			key: ByteBuffer.fromUTF8(key),
			bucket: ByteBuffer.fromUTF8(bucket),
			type: type
		}
	}

	childUpdate(key: colonyPB.ApbBoundObject, operation: colonyPB.ApbUpdateOperation): colonyPB.ApbUpdateOp {
		return this.makeUpdate({
			mapop: {
				updates: [{
					key: {
						key: key.key,
						type: key.type
					},
					update: operation
				}],
				removedKeys: []
			}
		});
	}

	makeUpdate(operation: colonyPB.ApbUpdateOperation): colonyPB.ApbUpdateOp {
		return this.parent.childUpdate(this.key, operation);
	}

	interpretReadResponse(readResponse: colonyPB.ApbReadObjectResp): any {
		let vals = readResponse.map!.entries!;
		return new CrdtMapValueImpl(this.parent, vals);
	}

	public async read(): Promise<CrdtMapValue> {
		let r = await this.parent.readBatch([this])
		return r[0]
	}

	getBucket(): string {
		return "";
	}

	public async readBatch(objects: colonyObject<any>[]): Promise<any[]> {
		let objects2 = objects as colonyObjectImpl<any>[]
		let r = await this.parent.readBatch([this])
		let map: CrdtMapValue = r[0];
		let values: any[] = [];
		// filter out the actual keys
		for (let obj of objects2) {
			values.push(map.get(obj.key.key!.toUTF8(), obj.key.type!));
		}
		return values;
	}

	public jsToBinary(obj: any): ByteBuffer {
		return this.parent.jsToBinary(obj);
	}

	public binaryToJs(byteBuffer: ByteBuffer): any {
		return this.parent.binaryToJs(byteBuffer);
	}


	public remove(object: colonyObject<any>): colonyPB.ApbUpdateOp {
		return this.removeAll([object]);
	}

	public removeAll(objects: colonyObject<any>[]): colonyPB.ApbUpdateOp {
		let objects2 = objects as colonyObjectImpl<any>[];
		let removedKeys: colonyPB.ApbMapKey[] = objects2.map(obj => {
			return {
				key: obj.key.key,
				type: obj.key.type
			};
		});
		return this.makeUpdate({
			mapop: {
				updates: [],
				removedKeys: removedKeys
			}
		})
	}

}