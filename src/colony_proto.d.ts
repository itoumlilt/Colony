declare module colonyPB {
	
	
	interface ProtoBufMapItem<KeyType, ValueType> {
		key : KeyType,
		value : ValueType
	}
	
	interface ProtoBufMap<KeyType, ValueType> {
		clear(): void;
		delete(key: KeyType): boolean;
		get(key: KeyType): ValueType;
		has(key: KeyType): boolean;
		set(key: KeyType, value: ValueType): void;
		forEach(fn: (value: ValueType, key?: KeyType) => void): void;
		size: number;
		map : { [key: string]: ProtoBufMapItem<KeyType, ValueType> }
	}
	
	export interface ProtoBufBuilder {
		ApbErrorResp: ApbErrorRespBuilder;
		ApbCounterUpdate: ApbCounterUpdateBuilder;
		ApbGetCounterResp: ApbGetCounterRespBuilder;
		ApbSetUpdate: ApbSetUpdateBuilder;
		ApbGetSetResp: ApbGetSetRespBuilder;
		ApbRegUpdate: ApbRegUpdateBuilder;
		ApbGetRegResp: ApbGetRegRespBuilder;
		ApbGetMVRegResp: ApbGetMVRegRespBuilder;
		ApbMapKey: ApbMapKeyBuilder;
		ApbMapUpdate: ApbMapUpdateBuilder;
		ApbMapNestedUpdate: ApbMapNestedUpdateBuilder;
		ApbGetMapResp: ApbGetMapRespBuilder;
		ApbMapEntry: ApbMapEntryBuilder;
		ApbFlagUpdate: ApbFlagUpdateBuilder;
		ApbGetFlagResp: ApbGetFlagRespBuilder;
		ApbCrdtReset: ApbCrdtResetBuilder;
		ApbOperationResp: ApbOperationRespBuilder;
		ApbTxnProperties: ApbTxnPropertiesBuilder;
		ApbBoundObject: ApbBoundObjectBuilder;
		ApbReadObjects: ApbReadObjectsBuilder;
		ApbUpdateOp: ApbUpdateOpBuilder;
		ApbUpdateOperation: ApbUpdateOperationBuilder;
		ApbUpdateObjects: ApbUpdateObjectsBuilder;
		ApbStartTransaction: ApbStartTransactionBuilder;
		ApbAbortTransaction: ApbAbortTransactionBuilder;
		ApbCommitTransaction: ApbCommitTransactionBuilder;
		ApbStaticUpdateObjects: ApbStaticUpdateObjectsBuilder;
		ApbStaticReadObjects: ApbStaticReadObjectsBuilder;
		ApbStartTransactionResp: ApbStartTransactionRespBuilder;
		ApbReadObjectResp: ApbReadObjectRespBuilder;
		ApbReadObjectsResp: ApbReadObjectsRespBuilder;
		ApbCommitResp: ApbCommitRespBuilder;
		ApbStaticReadObjectsResp: ApbStaticReadObjectsRespBuilder;
		CRDT_type: CRDT_type;
		
}
}

declare module colonyPB {

	export interface ApbErrorResp {

		

errmsg: ByteBuffer;
		




errcode: number;
		




}

	export interface ApbErrorRespMessage extends ApbErrorResp {
	add(key: string, value: any, noAssert?: boolean): ApbErrorRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbErrorRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbErrorRespBuilder {
	new(data?: ApbErrorResp): ApbErrorRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbErrorRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbErrorRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbErrorRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbErrorRespMessage;
	decode64(str: string) : ApbErrorRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbErrorRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbErrorRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbErrorRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbErrorRespMessage;
	decodeHex(str: string): ApbErrorRespMessage;
	decodeJSON(str: string): ApbErrorRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbCounterUpdate {

		

inc?: Long;
		




}

	export interface ApbCounterUpdateMessage extends ApbCounterUpdate {
	add(key: string, value: any, noAssert?: boolean): ApbCounterUpdateMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbCounterUpdateMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbCounterUpdateBuilder {
	new(data?: ApbCounterUpdate): ApbCounterUpdateMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbCounterUpdateMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbCounterUpdateMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbCounterUpdateMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbCounterUpdateMessage;
	decode64(str: string) : ApbCounterUpdateMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbCounterUpdateMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbCounterUpdateMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbCounterUpdateMessage;
	decodeDelimited(buffer: string, enc: string): ApbCounterUpdateMessage;
	decodeHex(str: string): ApbCounterUpdateMessage;
	decodeJSON(str: string): ApbCounterUpdateMessage;
	
}

}


declare module colonyPB {

	export interface ApbGetCounterResp {

		

value: number;
		




}

	export interface ApbGetCounterRespMessage extends ApbGetCounterResp {
	add(key: string, value: any, noAssert?: boolean): ApbGetCounterRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbGetCounterRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbGetCounterRespBuilder {
	new(data?: ApbGetCounterResp): ApbGetCounterRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbGetCounterRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbGetCounterRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbGetCounterRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbGetCounterRespMessage;
	decode64(str: string) : ApbGetCounterRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbGetCounterRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbGetCounterRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbGetCounterRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbGetCounterRespMessage;
	decodeHex(str: string): ApbGetCounterRespMessage;
	decodeJSON(str: string): ApbGetCounterRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbSetUpdate {

		

optype: ApbSetUpdate.SetOpType;
		




adds: ByteBuffer[];
		




rems: ByteBuffer[];
		




}

	export interface ApbSetUpdateMessage extends ApbSetUpdate {
	add(key: string, value: any, noAssert?: boolean): ApbSetUpdateMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbSetUpdateMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbSetUpdateBuilder {
	new(data?: ApbSetUpdate): ApbSetUpdateMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbSetUpdateMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbSetUpdateMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbSetUpdateMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbSetUpdateMessage;
	decode64(str: string) : ApbSetUpdateMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbSetUpdateMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbSetUpdateMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbSetUpdateMessage;
	decodeDelimited(buffer: string, enc: string): ApbSetUpdateMessage;
	decodeHex(str: string): ApbSetUpdateMessage;
	decodeJSON(str: string): ApbSetUpdateMessage;
	SetOpType: ApbSetUpdate.SetOpType;
	
}

}

declare module colonyPB.ApbSetUpdate {
	export const enum SetOpType {
		ADD = 1,
		REMOVE = 2,
		
}
}


declare module colonyPB {

	export interface ApbGetSetResp {

		

value: ByteBuffer[];
		




}

	export interface ApbGetSetRespMessage extends ApbGetSetResp {
	add(key: string, value: any, noAssert?: boolean): ApbGetSetRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbGetSetRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbGetSetRespBuilder {
	new(data?: ApbGetSetResp): ApbGetSetRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbGetSetRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbGetSetRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbGetSetRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbGetSetRespMessage;
	decode64(str: string) : ApbGetSetRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbGetSetRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbGetSetRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbGetSetRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbGetSetRespMessage;
	decodeHex(str: string): ApbGetSetRespMessage;
	decodeJSON(str: string): ApbGetSetRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbRegUpdate {

		

value: ByteBuffer;
		




}

	export interface ApbRegUpdateMessage extends ApbRegUpdate {
	add(key: string, value: any, noAssert?: boolean): ApbRegUpdateMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbRegUpdateMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbRegUpdateBuilder {
	new(data?: ApbRegUpdate): ApbRegUpdateMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbRegUpdateMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbRegUpdateMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbRegUpdateMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbRegUpdateMessage;
	decode64(str: string) : ApbRegUpdateMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbRegUpdateMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbRegUpdateMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbRegUpdateMessage;
	decodeDelimited(buffer: string, enc: string): ApbRegUpdateMessage;
	decodeHex(str: string): ApbRegUpdateMessage;
	decodeJSON(str: string): ApbRegUpdateMessage;
	
}

}


declare module colonyPB {

	export interface ApbGetRegResp {

		

value: ByteBuffer;
		




}

	export interface ApbGetRegRespMessage extends ApbGetRegResp {
	add(key: string, value: any, noAssert?: boolean): ApbGetRegRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbGetRegRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbGetRegRespBuilder {
	new(data?: ApbGetRegResp): ApbGetRegRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbGetRegRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbGetRegRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbGetRegRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbGetRegRespMessage;
	decode64(str: string) : ApbGetRegRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbGetRegRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbGetRegRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbGetRegRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbGetRegRespMessage;
	decodeHex(str: string): ApbGetRegRespMessage;
	decodeJSON(str: string): ApbGetRegRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbGetMVRegResp {

		

values: ByteBuffer[];
		




}

	export interface ApbGetMVRegRespMessage extends ApbGetMVRegResp {
	add(key: string, value: any, noAssert?: boolean): ApbGetMVRegRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbGetMVRegRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbGetMVRegRespBuilder {
	new(data?: ApbGetMVRegResp): ApbGetMVRegRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbGetMVRegRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbGetMVRegRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbGetMVRegRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbGetMVRegRespMessage;
	decode64(str: string) : ApbGetMVRegRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbGetMVRegRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbGetMVRegRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbGetMVRegRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbGetMVRegRespMessage;
	decodeHex(str: string): ApbGetMVRegRespMessage;
	decodeJSON(str: string): ApbGetMVRegRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbMapKey {

		

key: ByteBuffer;
		




type: CRDT_type;
		




}

	export interface ApbMapKeyMessage extends ApbMapKey {
	add(key: string, value: any, noAssert?: boolean): ApbMapKeyMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbMapKeyMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbMapKeyBuilder {
	new(data?: ApbMapKey): ApbMapKeyMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbMapKeyMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbMapKeyMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbMapKeyMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbMapKeyMessage;
	decode64(str: string) : ApbMapKeyMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbMapKeyMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbMapKeyMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbMapKeyMessage;
	decodeDelimited(buffer: string, enc: string): ApbMapKeyMessage;
	decodeHex(str: string): ApbMapKeyMessage;
	decodeJSON(str: string): ApbMapKeyMessage;
	
}

}


declare module colonyPB {

	export interface ApbMapUpdate {

		

updates: ApbMapNestedUpdate[];
		




removedKeys: ApbMapKey[];
		




}

	export interface ApbMapUpdateMessage extends ApbMapUpdate {
	add(key: string, value: any, noAssert?: boolean): ApbMapUpdateMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbMapUpdateMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbMapUpdateBuilder {
	new(data?: ApbMapUpdate): ApbMapUpdateMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbMapUpdateMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbMapUpdateMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbMapUpdateMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbMapUpdateMessage;
	decode64(str: string) : ApbMapUpdateMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbMapUpdateMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbMapUpdateMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbMapUpdateMessage;
	decodeDelimited(buffer: string, enc: string): ApbMapUpdateMessage;
	decodeHex(str: string): ApbMapUpdateMessage;
	decodeJSON(str: string): ApbMapUpdateMessage;
	
}

}


declare module colonyPB {

	export interface ApbMapNestedUpdate {

		

key: ApbMapKey;
		




update: ApbUpdateOperation;
		




}

	export interface ApbMapNestedUpdateMessage extends ApbMapNestedUpdate {
	add(key: string, value: any, noAssert?: boolean): ApbMapNestedUpdateMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbMapNestedUpdateMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbMapNestedUpdateBuilder {
	new(data?: ApbMapNestedUpdate): ApbMapNestedUpdateMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbMapNestedUpdateMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbMapNestedUpdateMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbMapNestedUpdateMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbMapNestedUpdateMessage;
	decode64(str: string) : ApbMapNestedUpdateMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbMapNestedUpdateMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbMapNestedUpdateMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbMapNestedUpdateMessage;
	decodeDelimited(buffer: string, enc: string): ApbMapNestedUpdateMessage;
	decodeHex(str: string): ApbMapNestedUpdateMessage;
	decodeJSON(str: string): ApbMapNestedUpdateMessage;
	
}

}


declare module colonyPB {

	export interface ApbGetMapResp {

		

entries: ApbMapEntry[];
		




}

	export interface ApbGetMapRespMessage extends ApbGetMapResp {
	add(key: string, value: any, noAssert?: boolean): ApbGetMapRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbGetMapRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbGetMapRespBuilder {
	new(data?: ApbGetMapResp): ApbGetMapRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbGetMapRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbGetMapRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbGetMapRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbGetMapRespMessage;
	decode64(str: string) : ApbGetMapRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbGetMapRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbGetMapRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbGetMapRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbGetMapRespMessage;
	decodeHex(str: string): ApbGetMapRespMessage;
	decodeJSON(str: string): ApbGetMapRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbMapEntry {

		

key: ApbMapKey;
		




value: ApbReadObjectResp;
		




}

	export interface ApbMapEntryMessage extends ApbMapEntry {
	add(key: string, value: any, noAssert?: boolean): ApbMapEntryMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbMapEntryMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbMapEntryBuilder {
	new(data?: ApbMapEntry): ApbMapEntryMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbMapEntryMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbMapEntryMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbMapEntryMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbMapEntryMessage;
	decode64(str: string) : ApbMapEntryMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbMapEntryMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbMapEntryMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbMapEntryMessage;
	decodeDelimited(buffer: string, enc: string): ApbMapEntryMessage;
	decodeHex(str: string): ApbMapEntryMessage;
	decodeJSON(str: string): ApbMapEntryMessage;
	
}

}


declare module colonyPB {

	export interface ApbFlagUpdate {

		

value: boolean;
		




}

	export interface ApbFlagUpdateMessage extends ApbFlagUpdate {
	add(key: string, value: any, noAssert?: boolean): ApbFlagUpdateMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbFlagUpdateMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbFlagUpdateBuilder {
	new(data?: ApbFlagUpdate): ApbFlagUpdateMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbFlagUpdateMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbFlagUpdateMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbFlagUpdateMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbFlagUpdateMessage;
	decode64(str: string) : ApbFlagUpdateMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbFlagUpdateMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbFlagUpdateMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbFlagUpdateMessage;
	decodeDelimited(buffer: string, enc: string): ApbFlagUpdateMessage;
	decodeHex(str: string): ApbFlagUpdateMessage;
	decodeJSON(str: string): ApbFlagUpdateMessage;
	
}

}


declare module colonyPB {

	export interface ApbGetFlagResp {

		

value: boolean;
		




}

	export interface ApbGetFlagRespMessage extends ApbGetFlagResp {
	add(key: string, value: any, noAssert?: boolean): ApbGetFlagRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbGetFlagRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbGetFlagRespBuilder {
	new(data?: ApbGetFlagResp): ApbGetFlagRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbGetFlagRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbGetFlagRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbGetFlagRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbGetFlagRespMessage;
	decode64(str: string) : ApbGetFlagRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbGetFlagRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbGetFlagRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbGetFlagRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbGetFlagRespMessage;
	decodeHex(str: string): ApbGetFlagRespMessage;
	decodeJSON(str: string): ApbGetFlagRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbCrdtReset {

		

}

	export interface ApbCrdtResetMessage extends ApbCrdtReset {
	add(key: string, value: any, noAssert?: boolean): ApbCrdtResetMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbCrdtResetMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbCrdtResetBuilder {
	new(data?: ApbCrdtReset): ApbCrdtResetMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbCrdtResetMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbCrdtResetMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbCrdtResetMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbCrdtResetMessage;
	decode64(str: string) : ApbCrdtResetMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbCrdtResetMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbCrdtResetMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbCrdtResetMessage;
	decodeDelimited(buffer: string, enc: string): ApbCrdtResetMessage;
	decodeHex(str: string): ApbCrdtResetMessage;
	decodeJSON(str: string): ApbCrdtResetMessage;
	
}

}


declare module colonyPB {

	export interface ApbOperationResp {

		

success: boolean;
		




errorcode?: number;
		




}

	export interface ApbOperationRespMessage extends ApbOperationResp {
	add(key: string, value: any, noAssert?: boolean): ApbOperationRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbOperationRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbOperationRespBuilder {
	new(data?: ApbOperationResp): ApbOperationRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbOperationRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbOperationRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbOperationRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbOperationRespMessage;
	decode64(str: string) : ApbOperationRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbOperationRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbOperationRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbOperationRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbOperationRespMessage;
	decodeHex(str: string): ApbOperationRespMessage;
	decodeJSON(str: string): ApbOperationRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbTxnProperties {

		

read_write?: number;
		




red_blue?: number;
		




}

	export interface ApbTxnPropertiesMessage extends ApbTxnProperties {
	add(key: string, value: any, noAssert?: boolean): ApbTxnPropertiesMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbTxnPropertiesMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbTxnPropertiesBuilder {
	new(data?: ApbTxnProperties): ApbTxnPropertiesMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbTxnPropertiesMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbTxnPropertiesMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbTxnPropertiesMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbTxnPropertiesMessage;
	decode64(str: string) : ApbTxnPropertiesMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbTxnPropertiesMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbTxnPropertiesMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbTxnPropertiesMessage;
	decodeDelimited(buffer: string, enc: string): ApbTxnPropertiesMessage;
	decodeHex(str: string): ApbTxnPropertiesMessage;
	decodeJSON(str: string): ApbTxnPropertiesMessage;
	
}

}


declare module colonyPB {

	export interface ApbBoundObject {

		

key: ByteBuffer;
		




type: CRDT_type;
		




bucket: ByteBuffer;
		




}

	export interface ApbBoundObjectMessage extends ApbBoundObject {
	add(key: string, value: any, noAssert?: boolean): ApbBoundObjectMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbBoundObjectMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbBoundObjectBuilder {
	new(data?: ApbBoundObject): ApbBoundObjectMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbBoundObjectMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbBoundObjectMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbBoundObjectMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbBoundObjectMessage;
	decode64(str: string) : ApbBoundObjectMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbBoundObjectMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbBoundObjectMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbBoundObjectMessage;
	decodeDelimited(buffer: string, enc: string): ApbBoundObjectMessage;
	decodeHex(str: string): ApbBoundObjectMessage;
	decodeJSON(str: string): ApbBoundObjectMessage;
	
}

}


declare module colonyPB {

	export interface ApbReadObjects {

		

boundobjects: ApbBoundObject[];
		




transaction_descriptor: ByteBuffer;
		




}

	export interface ApbReadObjectsMessage extends ApbReadObjects {
	add(key: string, value: any, noAssert?: boolean): ApbReadObjectsMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbReadObjectsMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbReadObjectsBuilder {
	new(data?: ApbReadObjects): ApbReadObjectsMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbReadObjectsMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbReadObjectsMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbReadObjectsMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbReadObjectsMessage;
	decode64(str: string) : ApbReadObjectsMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbReadObjectsMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbReadObjectsMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbReadObjectsMessage;
	decodeDelimited(buffer: string, enc: string): ApbReadObjectsMessage;
	decodeHex(str: string): ApbReadObjectsMessage;
	decodeJSON(str: string): ApbReadObjectsMessage;
	
}

}


declare module colonyPB {

	export interface ApbUpdateOp {

		

boundobject: ApbBoundObject;
		




operation: ApbUpdateOperation;
		




}

	export interface ApbUpdateOpMessage extends ApbUpdateOp {
	add(key: string, value: any, noAssert?: boolean): ApbUpdateOpMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbUpdateOpMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbUpdateOpBuilder {
	new(data?: ApbUpdateOp): ApbUpdateOpMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbUpdateOpMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbUpdateOpMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbUpdateOpMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbUpdateOpMessage;
	decode64(str: string) : ApbUpdateOpMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbUpdateOpMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbUpdateOpMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbUpdateOpMessage;
	decodeDelimited(buffer: string, enc: string): ApbUpdateOpMessage;
	decodeHex(str: string): ApbUpdateOpMessage;
	decodeJSON(str: string): ApbUpdateOpMessage;
	
}

}


declare module colonyPB {

	export interface ApbUpdateOperation {

		

counterop?: ApbCounterUpdate;
		




setop?: ApbSetUpdate;
		




regop?: ApbRegUpdate;
		




mapop?: ApbMapUpdate;
		




resetop?: ApbCrdtReset;
		




flagop?: ApbFlagUpdate;
		




}

	export interface ApbUpdateOperationMessage extends ApbUpdateOperation {
	add(key: string, value: any, noAssert?: boolean): ApbUpdateOperationMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbUpdateOperationMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbUpdateOperationBuilder {
	new(data?: ApbUpdateOperation): ApbUpdateOperationMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbUpdateOperationMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbUpdateOperationMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbUpdateOperationMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbUpdateOperationMessage;
	decode64(str: string) : ApbUpdateOperationMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbUpdateOperationMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbUpdateOperationMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbUpdateOperationMessage;
	decodeDelimited(buffer: string, enc: string): ApbUpdateOperationMessage;
	decodeHex(str: string): ApbUpdateOperationMessage;
	decodeJSON(str: string): ApbUpdateOperationMessage;
	
}

}


declare module colonyPB {

	export interface ApbUpdateObjects {

		

updates: ApbUpdateOp[];
		




transaction_descriptor: ByteBuffer;
		




}

	export interface ApbUpdateObjectsMessage extends ApbUpdateObjects {
	add(key: string, value: any, noAssert?: boolean): ApbUpdateObjectsMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbUpdateObjectsMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbUpdateObjectsBuilder {
	new(data?: ApbUpdateObjects): ApbUpdateObjectsMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbUpdateObjectsMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbUpdateObjectsMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbUpdateObjectsMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbUpdateObjectsMessage;
	decode64(str: string) : ApbUpdateObjectsMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbUpdateObjectsMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbUpdateObjectsMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbUpdateObjectsMessage;
	decodeDelimited(buffer: string, enc: string): ApbUpdateObjectsMessage;
	decodeHex(str: string): ApbUpdateObjectsMessage;
	decodeJSON(str: string): ApbUpdateObjectsMessage;
	
}

}


declare module colonyPB {

	export interface ApbStartTransaction {

		

timestamp?: ByteBuffer;
		




properties?: ApbTxnProperties;
		




}

	export interface ApbStartTransactionMessage extends ApbStartTransaction {
	add(key: string, value: any, noAssert?: boolean): ApbStartTransactionMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbStartTransactionMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbStartTransactionBuilder {
	new(data?: ApbStartTransaction): ApbStartTransactionMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbStartTransactionMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbStartTransactionMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbStartTransactionMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbStartTransactionMessage;
	decode64(str: string) : ApbStartTransactionMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbStartTransactionMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbStartTransactionMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbStartTransactionMessage;
	decodeDelimited(buffer: string, enc: string): ApbStartTransactionMessage;
	decodeHex(str: string): ApbStartTransactionMessage;
	decodeJSON(str: string): ApbStartTransactionMessage;
	
}

}


declare module colonyPB {

	export interface ApbAbortTransaction {

		

transaction_descriptor: ByteBuffer;
		




}

	export interface ApbAbortTransactionMessage extends ApbAbortTransaction {
	add(key: string, value: any, noAssert?: boolean): ApbAbortTransactionMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbAbortTransactionMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbAbortTransactionBuilder {
	new(data?: ApbAbortTransaction): ApbAbortTransactionMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbAbortTransactionMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbAbortTransactionMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbAbortTransactionMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbAbortTransactionMessage;
	decode64(str: string) : ApbAbortTransactionMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbAbortTransactionMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbAbortTransactionMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbAbortTransactionMessage;
	decodeDelimited(buffer: string, enc: string): ApbAbortTransactionMessage;
	decodeHex(str: string): ApbAbortTransactionMessage;
	decodeJSON(str: string): ApbAbortTransactionMessage;
	
}

}


declare module colonyPB {

	export interface ApbCommitTransaction {

		

transaction_descriptor: ByteBuffer;
		




}

	export interface ApbCommitTransactionMessage extends ApbCommitTransaction {
	add(key: string, value: any, noAssert?: boolean): ApbCommitTransactionMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbCommitTransactionMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbCommitTransactionBuilder {
	new(data?: ApbCommitTransaction): ApbCommitTransactionMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbCommitTransactionMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbCommitTransactionMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbCommitTransactionMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbCommitTransactionMessage;
	decode64(str: string) : ApbCommitTransactionMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbCommitTransactionMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbCommitTransactionMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbCommitTransactionMessage;
	decodeDelimited(buffer: string, enc: string): ApbCommitTransactionMessage;
	decodeHex(str: string): ApbCommitTransactionMessage;
	decodeJSON(str: string): ApbCommitTransactionMessage;
	
}

}


declare module colonyPB {

	export interface ApbStaticUpdateObjects {

		

transaction: ApbStartTransaction;
		




updates: ApbUpdateOp[];
		




}

	export interface ApbStaticUpdateObjectsMessage extends ApbStaticUpdateObjects {
	add(key: string, value: any, noAssert?: boolean): ApbStaticUpdateObjectsMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbStaticUpdateObjectsMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbStaticUpdateObjectsBuilder {
	new(data?: ApbStaticUpdateObjects): ApbStaticUpdateObjectsMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbStaticUpdateObjectsMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbStaticUpdateObjectsMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbStaticUpdateObjectsMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbStaticUpdateObjectsMessage;
	decode64(str: string) : ApbStaticUpdateObjectsMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbStaticUpdateObjectsMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbStaticUpdateObjectsMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbStaticUpdateObjectsMessage;
	decodeDelimited(buffer: string, enc: string): ApbStaticUpdateObjectsMessage;
	decodeHex(str: string): ApbStaticUpdateObjectsMessage;
	decodeJSON(str: string): ApbStaticUpdateObjectsMessage;
	
}

}


declare module colonyPB {

	export interface ApbStaticReadObjects {

		

transaction: ApbStartTransaction;
		




objects: ApbBoundObject[];
		




}

	export interface ApbStaticReadObjectsMessage extends ApbStaticReadObjects {
	add(key: string, value: any, noAssert?: boolean): ApbStaticReadObjectsMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbStaticReadObjectsMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbStaticReadObjectsBuilder {
	new(data?: ApbStaticReadObjects): ApbStaticReadObjectsMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbStaticReadObjectsMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbStaticReadObjectsMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbStaticReadObjectsMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbStaticReadObjectsMessage;
	decode64(str: string) : ApbStaticReadObjectsMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbStaticReadObjectsMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbStaticReadObjectsMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbStaticReadObjectsMessage;
	decodeDelimited(buffer: string, enc: string): ApbStaticReadObjectsMessage;
	decodeHex(str: string): ApbStaticReadObjectsMessage;
	decodeJSON(str: string): ApbStaticReadObjectsMessage;
	
}

}


declare module colonyPB {

	export interface ApbStartTransactionResp {

		

success: boolean;
		




transaction_descriptor?: ByteBuffer;
		




errorcode?: number;
		




}

	export interface ApbStartTransactionRespMessage extends ApbStartTransactionResp {
	add(key: string, value: any, noAssert?: boolean): ApbStartTransactionRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbStartTransactionRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbStartTransactionRespBuilder {
	new(data?: ApbStartTransactionResp): ApbStartTransactionRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbStartTransactionRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbStartTransactionRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbStartTransactionRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbStartTransactionRespMessage;
	decode64(str: string) : ApbStartTransactionRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbStartTransactionRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbStartTransactionRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbStartTransactionRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbStartTransactionRespMessage;
	decodeHex(str: string): ApbStartTransactionRespMessage;
	decodeJSON(str: string): ApbStartTransactionRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbReadObjectResp {

		

counter?: ApbGetCounterResp;
		




set?: ApbGetSetResp;
		




reg?: ApbGetRegResp;
		




mvreg?: ApbGetMVRegResp;
		




map?: ApbGetMapResp;
		




flag?: ApbGetFlagResp;
		




}

	export interface ApbReadObjectRespMessage extends ApbReadObjectResp {
	add(key: string, value: any, noAssert?: boolean): ApbReadObjectRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	//set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbReadObjectRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbReadObjectRespBuilder {
	new(data?: ApbReadObjectResp): ApbReadObjectRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbReadObjectRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbReadObjectRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbReadObjectRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbReadObjectRespMessage;
	decode64(str: string) : ApbReadObjectRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbReadObjectRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbReadObjectRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbReadObjectRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbReadObjectRespMessage;
	decodeHex(str: string): ApbReadObjectRespMessage;
	decodeJSON(str: string): ApbReadObjectRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbReadObjectsResp {

		

success: boolean;
		




objects: ApbReadObjectResp[];
		




errorcode?: number;
		




}

	export interface ApbReadObjectsRespMessage extends ApbReadObjectsResp {
	add(key: string, value: any, noAssert?: boolean): ApbReadObjectsRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbReadObjectsRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbReadObjectsRespBuilder {
	new(data?: ApbReadObjectsResp): ApbReadObjectsRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbReadObjectsRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbReadObjectsRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbReadObjectsRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbReadObjectsRespMessage;
	decode64(str: string) : ApbReadObjectsRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbReadObjectsRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbReadObjectsRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbReadObjectsRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbReadObjectsRespMessage;
	decodeHex(str: string): ApbReadObjectsRespMessage;
	decodeJSON(str: string): ApbReadObjectsRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbCommitResp {

		

success: boolean;
		




commit_time?: ByteBuffer;
		




errorcode?: number;
		




}

	export interface ApbCommitRespMessage extends ApbCommitResp {
	add(key: string, value: any, noAssert?: boolean): ApbCommitRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbCommitRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbCommitRespBuilder {
	new(data?: ApbCommitResp): ApbCommitRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbCommitRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbCommitRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbCommitRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbCommitRespMessage;
	decode64(str: string) : ApbCommitRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbCommitRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbCommitRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbCommitRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbCommitRespMessage;
	decodeHex(str: string): ApbCommitRespMessage;
	decodeJSON(str: string): ApbCommitRespMessage;
	
}

}


declare module colonyPB {

	export interface ApbStaticReadObjectsResp {

		

objects: ApbReadObjectsResp;
		




committime: ApbCommitResp;
		




}

	export interface ApbStaticReadObjectsRespMessage extends ApbStaticReadObjectsResp {
	add(key: string, value: any, noAssert?: boolean): ApbStaticReadObjectsRespMessage;
	calculate(): number;
	encode64(): string;
	encodeAB(): ArrayBuffer;
	encodeDelimited(buffer?: ByteBuffer, noVerify?: boolean): ByteBuffer;
	encodeDelimited(buffer?: boolean, noVerify?: boolean): ByteBuffer;
	encodeHex(): string;
	encodeJSON(): string;
	encodeNB(): Buffer;
	get(key: string, noAssert: boolean): any;
	set(keyOrObj: string, value: any | boolean, noAssert: boolean): ApbStaticReadObjectsRespMessage;
	toArrayBuffer(): ArrayBuffer;
	toBase64(): string;
	toBuffer(): Buffer;
	toHex(): string;
	toRaw(): any;
	toString(): string;
}

export interface ApbStaticReadObjectsRespBuilder {
	new(data?: ApbStaticReadObjectsResp): ApbStaticReadObjectsRespMessage;
	decode(buffer: ArrayBuffer, length?: number | string, enc?: string) : ApbStaticReadObjectsRespMessage;
	decode(buffer: ByteBuffer, length?: number | string, enc?: string) : ApbStaticReadObjectsRespMessage;
	decode(buffer: Buffer, length?: number | string, enc?: string) : ApbStaticReadObjectsRespMessage;
	decode(buffer: string, length?: number | string, enc?: string) : ApbStaticReadObjectsRespMessage;
	decode64(str: string) : ApbStaticReadObjectsRespMessage;
	decodeDelimited(buffer: ArrayBuffer, enc: string): ApbStaticReadObjectsRespMessage;
	decodeDelimited(buffer: ByteBuffer, enc: string): ApbStaticReadObjectsRespMessage;
	decodeDelimited(buffer: Buffer, enc: string): ApbStaticReadObjectsRespMessage;
	decodeDelimited(buffer: string, enc: string): ApbStaticReadObjectsRespMessage;
	decodeHex(str: string): ApbStaticReadObjectsRespMessage;
	decodeJSON(str: string): ApbStaticReadObjectsRespMessage;
	
}

}


declare module colonyPB {
	export const enum CRDT_type {
		COUNTER = 3,
		ORSET = 4,
		LWWREG = 5,
		MVREG = 6,
		GMAP = 8,
		RWSET = 10,
		RRMAP = 11,
		FATCOUNTER = 12,
		FLAG_EW = 13,
		FLAG_DW = 14,
		
}
}


