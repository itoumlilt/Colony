import '../proto/colony_proto'
import path = require('path');

export module MessageCodes {
	export const apbRegUpdate = 107;
	export const apbGetRegResp = 108;
	export const apbCounterUpdate = 109;
	export const apbGetCounterResp = 110;
	export const apbOperationResp = 111;
	export const apbSetUpdate = 112;
	export const apbGetSetResp = 113;
	export const apbTxnProperties = 114;
	export const apbBoundObject = 115;
	export const apbReadObjects = 116;
	export const apbUpdateOp = 117;
	export const apbUpdateObjects = 118;
	export const apbStartTransaction = 119;
	export const apbAbortTransaction = 120;
	export const apbCommitTransaction = 121;
	export const apbStaticUpdateObjects = 122;
	export const apbStaticReadObjects = 123;
	export const apbStartTransactionResp = 124;
	export const apbReadObjectResp = 125;
	export const apbReadObjectsResp = 126;
	export const apbCommitResp = 127;
	export const apbStaticReadObjectsResp = 128;

	export var ProtoBuf = require("protobufjs");
	var colonyProtoSrc = path.join(__dirname, '..', 'proto', 'colony.proto');
	export var colonyPb: colonyPB.ProtoBufBuilder = ProtoBuf.protoFromFile(colonyProtoSrc).build("colonyPB");

	export function messageCodeToProto(code: number): any {
		switch (code) {
			case apbRegUpdate:
				return colonyPb.ApbRegUpdate;
			case apbGetRegResp:
				return colonyPb.ApbGetRegResp;
			case apbCounterUpdate:
				return colonyPb.ApbCounterUpdate;
			case apbGetCounterResp:
				return colonyPb.ApbGetCounterResp;
			case apbOperationResp:
				return colonyPb.ApbOperationResp;
			case apbSetUpdate:
				return colonyPb.ApbSetUpdate;
			case apbGetSetResp:
				return colonyPb.ApbGetSetResp;
			case apbTxnProperties:
				return colonyPb.ApbTxnProperties;
			case apbBoundObject:
				return colonyPb.ApbBoundObject;
			case apbReadObjects:
				return colonyPb.ApbReadObjects;
			case apbUpdateOp:
				return colonyPb.ApbUpdateOp;
			case apbUpdateObjects:
				return colonyPb.ApbUpdateObjects;
			case apbStartTransaction:
				return colonyPb.ApbStartTransaction;
			case apbAbortTransaction:
				return colonyPb.ApbAbortTransaction;
			case apbCommitTransaction:
				return colonyPb.ApbCommitTransaction;
			case apbStaticUpdateObjects:
				return colonyPb.ApbStaticUpdateObjects;
			case apbStaticReadObjects:
				return colonyPb.ApbStaticReadObjects;
			case apbStartTransactionResp:
				return colonyPb.ApbStartTransactionResp;
			case apbReadObjectResp:
				return colonyPb.ApbReadObjectResp;
			case apbReadObjectsResp:
				return colonyPb.ApbReadObjectsResp;
			case apbCommitResp:
				return colonyPb.ApbCommitResp;
			case apbStaticReadObjectsResp:
				return colonyPb.ApbStaticReadObjectsResp
		}
		throw new Error(`invalid code: ${code}`);
	}

}