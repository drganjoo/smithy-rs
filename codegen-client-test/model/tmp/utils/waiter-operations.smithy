$version: "2"
namespace com.test

@aws.protocols#awsJson1_0
service WaitersTestService {
    operations: [
        GetPrimitive,
        GetBooleanLogic,
        GetListOperation,
        GetStringEquals,
        GetMultiSelectList,
        GetMultiSelectHash,
        GetFunctionContains,
        GetFunctionLength,
        GetFunctionAbs,
        GetFunctionFloor,
        GetFunctionCeil,
        GetSubFieldProjection,
        GetFunctionSumEquals,
        GetFunctionAvgEquals,
        GetFunctionJoinEquals,
        GetFunctionStartsWithEquals,
        GetFunctionEndsWithEquals,
        GetFunctionKeysEquals,
        GetFunctionValuesEquals,
        GetFunctionMergeEquals,
        GetFunctionMaxEquals,
        GetFunctionMinEquals,
        GetFunctionReverseEquals,
        GetFunctionNotNullEquals,
        GetFunctionToArrayEquals,
        GetFunctionToStringEquals,
        GetFunctionToNumberEquals,
        GetFunctionTypeEquals,
        GetFunctionSortByEquals,
        GetFunctionSortEquals,
        GetFunctionMaxByEquals,
        GetFunctionMinByEquals,
        GetFunctionMapEquals,
    ]
}
